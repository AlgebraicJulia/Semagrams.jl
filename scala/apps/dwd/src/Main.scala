package semagrams.dwd

import semagrams.Viewport
import semagrams.api._
import semagrams.dblClickOn
import semagrams.acsets.{_, given}

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._
import semagrams.Background
import semagrams.EntityCollection

import scala.language.implicitConversions
import scala.reflect.ClassTag
import semagrams.sprites.{AltDPBox, BasicPort}

case object Box extends Ob {
  override val schema = SchBox
}

case object OutPort extends Ob
case object InPort extends Ob

case object SchBox extends Schema {
  val obs = Seq(OutPort, InPort)
  val homs = Seq()
  val attrs = Seq()
}

case object Wire extends Ob

case object Src extends Hom {
  val doms = Seq(PartType(Seq(Wire)))
  val codoms = Seq(
    PartType(Seq(Box, OutPort)),
    PartType(Seq(InPort))
  )
}

case object Tgt extends Hom {
  val doms = Seq(PartType(Seq(Wire)))
  val codoms = Seq(
    PartType(Seq(Box, InPort)),
    PartType(Seq(OutPort))
  )
}

case object SchDWD extends Schema {
  val obs = Seq(Box, OutPort, InPort, Wire)
  val homs = Seq(Src, Tgt)
  val attrs = Seq()
}

object DWD {
  def apply() = ACSet(SchDWD)
}

def bindings(
    es: EditorState,
    g: UndoableVar[ACSet],
    ui: UIState,
    vp: Viewport
) = {

  def dwdFromJson(s: String): Option[ACSet] = Some(DWD())

  def jsonFromDWD(dwd: ACSet): String = g.now().toString()

  val a = Actions(es, g, ui, jsonFromDWD, dwdFromJson)

  val boxMenu = Seq(
    (
      "Rename",
      (b: Part) => IO(println("rename fired")).>>(a.edit(Content, true)(b))
    ),
    ("Add Input", (b: Part) => IO(println("addIn")))
  )

  val wireMenu = Seq(
    (
      "Rename",
      (b: Part) => IO(println("rename fired")).>>(a.edit(Content, true)(b))
    )
  )

  def getBgPortType() = for {
    z <- es.mousePos
    c = es.size.now() / 2
    ptype =
      if (z - c).x < 0
      then InPort
      else OutPort
  } yield ptype

  def getBgPortInfo = for
    ptype <- getBgPortType()
    nports = g
      .now()
      .partsMap
      .get(ptype)
      .map(pts => pts.nextId)
      .getOrElse(0)
    size = es.size.now()
    p <- es.mousePos
  yield (ptype, portNumber(p, size, nports))

  def portNumber(pos: Complex, size: Complex, nports: Int) =
    val l = (0 to nports + 1).map(
      _ * size.y / (nports + 1)
    )
    l.filter(_ < pos.y).length

  import MouseButton._
  import KeyModifier._

  def portByPos(p: Part, pos: Complex): PartType =
    val obseq = p.ty match
      case ROOT.ty =>
        val sz = es.size.now()
        if pos.x < (sz.x / 2.0)
        then Seq(InPort)
        else Seq(OutPort)
      case PartType(Seq(Box)) =>
        val ctr = g.now().trySubpart(Center, p.head)
        ctr match
          case None => Seq(InPort)
          case Some(c) =>
            if pos.x < c.x
            then Seq(InPort)
            else Seq(OutPort)
      case _ => Seq()
    PartType(obseq)

  Seq(
    // Add box
    dblClickOnPart(MouseButton.Left, ROOT.ty)
      .flatMap(p =>
        for
          b <- a.addAtMouse(Box)
          _ <- a.edit(Content, true)(b)
        yield ()
      ),
    dblClickOnPart(MouseButton.Left, PartType(Seq(Box)))
      .flatMap(b =>
        // println(s"dbl box $b")
        a.edit(Content, true)(b)
      ),
    clickOnPart(Left)
      .withMods()
      .flatMap(p =>
        // println(s"clickOnPart $p")
        p.ty match
          case ROOT.ty =>
            es.mousePos.flatMap(z => IO(()) // IO(println(s"clicked at $z"))
            )
          case PartType(Seq(Box, _*)) => a.dragMove(p.head)
      ),
    clickOnPart(Left)
      .withMods(Shift)
      .flatMap(p =>
        // println(s"shiftclick: $p, ${p == ROOT}")
        p.ty match
          case ROOT.ty => a.dragEdge(Wire, Src, Tgt, portByPos)(ROOT)
          case PartType(Seq(Box, _*)) =>
            a.dragEdge(Wire, Src, Tgt, portByPos)(p)
      ),
    menuOnPart(PartType(Seq(Box))).flatMap(es.makeMenu(ui, boxMenu)),
    menuOnPart(PartType(Seq(Wire))).flatMap(es.makeMenu(ui, wireMenu)),
    keyDown("d").andThen(a.del),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    // to remove
    keyDown("?").andThen(a.debug),
    keyDown("s").andThen(a.importExport)
  )
}

def layoutPorts(dims: Complex, init: ACSet): ACSet = {
  import Complex.im
  def helper(acs: ACSet, portOb: Ob, dir: (-1) | 1): ACSet = {
    val ports = acs.parts(ROOT, portOb)
    val sideCenter = dims / 2 + (dir * dims.x / 2)
    val spacer = FixedRangeExceptEnds(-dims.y / 2, dims.y / 2)
    val n = ports.length
    val cs = ports.zipWithIndex.map(
      { case ((p, sub), i) =>
        (p, sideCenter + spacer.assignPos(i, n) * im)
      }
    )
    cs.foldLeft(acs)((acs, pc) => acs.setSubpart(pc._1, Center, pc._2))
  }
  helper(helper(init, InPort, -1), OutPort, 1)
}

def wireProps(
    src: Hom,
    tgt: Hom
)(_e: Entity, acs: ACSet, m: EntityMap): PropMap = {
  val p = acs.props
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s.flatMap(findCenter(_, m)).getOrElse(p(Start))
  val tpos = t.flatMap(findCenter(_, m)).getOrElse(p(End))

  def getDir(s: Option[Part]) = s match
    case Some(p)
        if p.ty.path == Seq(Box, OutPort) |
          p.ty.path == Seq(InPort) =>
      10.0
    case Some(p)
        if p.ty.path == Seq(Box, InPort) |
          p.ty.path == Seq(OutPort) =>
      -10.0
    case _ => 0.0

  val (startDir, endDir) = (getDir(s), getDir(t))

  PropMap().set(WireProp.StartDir, startDir).set(WireProp.EndDir, endDir)
    + (Start, spos) + (End, tpos) + (Stroke, "blue")

}

def ACSetWireSource(
    ob: Ob,
    src: Hom,
    tgt: Hom,
    sprite: Sprite
) = ACSetEntitySource(ob, sprite).addPropsBy(wireProps(src, tgt))

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = DWD()
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal).map(layoutPorts)
        )
        vp <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, AltDPBox(InPort, OutPort)(es))
              .withProps(PropMap() + (FontSize, 22)),
            ACSetEntitySource(InPort, BasicPort()(es))
              .withProps(PropMap() + (MinimumWidth, 40) + (Fill, "black")),
            ACSetEntitySource(OutPort, BasicPort()(es))
              .withProps(PropMap() + (MinimumWidth, 40) + (Fill, "black")),
            ACSetWireSource(Wire, Src, Tgt, BasicWire(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui, vp))
      } yield ()
    }
  }
}
