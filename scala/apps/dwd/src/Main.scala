package semagrams.dwd

import semagrams.api._
import semagrams.acsets.{_, given}

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

case object SchDWD extends Schema {

  val obs = Seq(DWDObs.values*)
  val homs = Seq(DWDHoms.values*)
  val attrs = Seq(DWDAttrs.values*)

  enum DWDObs(_schema: Schema = SchEmpty) extends Ob:
    override lazy val schema = _schema
    case InPort, OutPort, Wire
    case Box extends DWDObs(SchDWD)

  import DWDObs._

  enum DWDHoms(val doms: Seq[PartType], val codoms: Seq[PartType]) extends Hom:
    case Src
        extends DWDHoms(
          Wire.asDom(),
          InPort.asDom() :+ Box.extend(OutPort)
        )
    case Tgt
        extends DWDHoms(
          Wire.asDom(),
          OutPort.asDom() :+ Box.extend(InPort)
        )

  enum DWDAttrs(val doms: Seq[PartType]) extends Attr:
    case PortType
        extends DWDAttrs(
          Wire.asDom() ++ InPort.asDom() ++ OutPort.asDom()
            :+ Box.extend(InPort) :+ Box.extend(OutPort)
        )
        with PValue[DWDType]

  enum DWDType derives ReadWriter:
    case Person, Document, Standard, Requirement

    def props = this match
      case Person      => PropMap() + (Fill, "red") + (Stroke, "red")
      case Document    => PropMap() + (Fill, "blue") + (Stroke, "blue")
      case Standard    => PropMap() + (Fill, "green") + (Stroke, "green")
      case Requirement => PropMap() + (Fill, "purple") + (Stroke, "purple")

}

import SchDWD._
import DWDObs._
import DWDHoms._
import DWDAttrs._

def bindings(
    es: EditorState,
    g: UndoableVar[ACSet],
    ui: UIState,
    vp: Viewport
) = {

  val a = Actions(es, g, ui)

  /** Select a new position (in/out + index) based on the mouse position */
  def liftPort(p: Part, pos: Complex): Seq[(Ob, Int)] =
    val ext = p - es.bgPart
    val (sz, c) = ext.ty.path match
      case Seq() =>
        (
          es.size.now(),
          es.size.now() / 2.0
        )
      case Seq(Box) =>
        (
          a.getDims(p),
          g.now()
            .trySubpart(Center, p)
            .getOrElse(
              throw msgError(s"missing center for $p")
            )
        )
      case _ =>
        return Seq()

    val ptype = if pos.x < c.x then InPort else OutPort

    val nports = g.now().parts(p, ptype).length
    val j = DPBox.portNumber(pos - c + (sz / 2.0), sz, nports)
    Seq((ptype, j))

  /** Set the type of `p` and any other ports or wires connected to it */
  def setType(p: Part, tpe: DWDType): IO[Unit] =
    p.lastOb match
      case Wire => (
        for
          _ <- a.set(p, PortType, tpe)
          s = g.now().trySubpart(Src, p)
          t = g.now().trySubpart(Tgt, p)
          _ <- s match
            case Some(p) if g.now().trySubpart(PortType, p) != Some(tpe) =>
              setType(p, tpe)
            case _ => IO(())
          _ <- t match
            case Some(p) if g.now().trySubpart(PortType, p) != Some(tpe) =>
              setType(p, tpe)
            case _ => IO(())
        yield ()
      )
      case InPort | OutPort => (
        for
          _ <- a.set(p, PortType, tpe)
          ws = (g.now().incident(p, Src) ++ g.now().incident(p, Tgt))
            .filter(w => g.now().trySubpart(PortType, w) != Some(tpe))
          _ <- a.doAll(ws.map(setType(_, tpe)))
        yield ()
      )

  /** Set the types of `p` and `q` to be equals, unless both are defined and
    * different, in which case die.
    */
  def eqTypes(p: Part, q: Part): IO[Unit] =
    val ptpe = g.now().trySubpart(PortType, p)
    val qtpe = g.now().trySubpart(PortType, q)
    (ptpe, qtpe) match
      case (Some(tpe1), Some(tpe2)) =>
        if tpe1 == tpe2
        then IO(())
        else a.die
      case (Some(tpe), None) => setType(q, tpe)
      case (None, Some(tpe)) => setType(p, tpe)
      case (None, None)      => IO(())

  def test(p: Part) = es.mousePos.map(z => println(s"test: mousePos = $z"))

  val layout = DPBox.layoutPortsBg(InPort, OutPort)

  import MouseButton._
  import KeyModifier._

  val helpText = """
    ∙ Double click  = add box/edit labels
    ∙ Drag = Drag box
    ∙ Drag + Shift = Add/unplug wire
    ∙ Double click + Ctrl = Zoom into/out of boxes
    ∙ "d" = Delete element below mouse
    ∙ "s" = Open import/export window
    ∙ Ctrl + "z"/Shift + Ctrl + "Z" = undo/redo
  """

  val boxMenu = Seq(
    (
      "Rename",
      (b: Part) => IO(println("rename fired")).>>(a.edit(Content, true)(b))
    )
  )

  def menuItem(tpe: DWDType) = (
    s"Set type to $tpe",
    (p: Part) => setType(es.bgPlus(p), tpe)
  )

  val wireMenu: Seq[(String, Part => IO[Unit])] =
    DWDType.values.toSeq.map(menuItem)

  Seq(
    // test hovered part
    keyDown("t").andThen(fromMaybe(es.hoveredPart).flatMap(test)),

    // Add box at mouse
    dblClickOnPart(Left, ROOT.ty)
      .withMods()
      .flatMap(_ =>
        for
          b <- a.addAtMouse(Box)
          _ <- a.edit(Content, true)(b)
        yield b
      ),

    // Edit box label
    dblClickOnPart(Left)
      .withMods()
      .map(b => es.bgPlus(b))
      .flatMap(a.edit(Content, true)),

    // Zoom into box
    dblClickOnPart(Left, PartType(Seq(Box)))
      //.withMods(Ctrl)
      .withAltMods(Set(Ctrl), Set(Meta))
      .map(b => es.bgPlus(b))
      .flatMap(b => a.zoomIn(b, layout, entitySources)),

    // Zoom out of box
    dblClickOn(Left)
      //.withMods(Ctrl)
      .withAltMods(Set(Ctrl), Set(Meta))
      .flatMap(_ => a.zoomOut(layout, entitySources)),

    // Drag box
    clickOnPart(Left)
      .withMods()
      .flatMap(p =>
        p.ty match
          case ROOT.ty                => es.mousePos.flatMap(z => IO(()))
          case PartType(Seq(Box, _*)) => a.dragMove(p.head)
      ),

    // Drag wire (or unplug)
    clickOnPart(Left)
      .withMods(Shift)
      .map(es.bgPlus(_))
      .flatMap(p =>
        val wires = (g.now().incident(p, Src) ++ g.now().incident(p, Tgt))
          .filter(_.ty == es.bgPart.ty.extend(Wire))

        if wires.isEmpty
        then
          (for
            w <- a.dragEdge(Wire, Src, Tgt, liftPort, eqTypes)(p)
            _ <- a.edit(Content, true)(w)
          yield w)
        else a.unplug(p, wires(0), Src, Tgt, liftPort, eqTypes)
      ),

    // Menu actions
    clickOnPart(Right).flatMap(p =>
      p.lastOb match
        case Wire | InPort | OutPort =>
          es.makeMenu(ui, wireMenu)(p)
        case Box =>
          es.makeMenu(ui, boxMenu)(p)
        case _ => a.die
    ),

    // Delete part
    keyDown("d").andThen(a.del),

    // Undo
    keyDown("z")
      //.withMods(Ctrl)
      .withAltMods(Set(Ctrl), Set(Meta))
      .andThen(IO(g.undo())),

    // Redo
    keyDown("Z")
      //.withMods(Ctrl, Shift)
      .withAltMods(Set(Ctrl, Shift), Set(Meta, Shift))
      .andThen(IO(g.redo())),

    // Print current state
    keyDown("?").andThen(a.debug),

    // Open serialization window
    keyDown("s").andThen(a.importExport),

    // Open serialization window
    keyDown("w").andThen(
      a.exportTikz(
        Seq(Box, InPort, OutPort, Wire),
        Seq(InPort, OutPort)
      )
    )
  )
}

def portDir(p: Part) = p.ty.path match {
  case Seq(Box, OutPort) | Seq(InPort) => 10.0
  case Seq(Box, InPort) | Seq(OutPort) => -10.0
  case _                               => 0.0
}

def style(acs: ACSet, p: Part): PropMap = (p.lastOb match
  case Wire             => acs.props.get(PortType)
  case InPort | OutPort => acs.props.get(PortType)
  case _                => None
).map(_.props).getOrElse(PropMap())

val entitySources = (es: EditorState) =>
  Seq(
    ACSetEntitySource(Box, AltDPBox(InPort, OutPort, style)(es))
      .withProps(PropMap() + (FontSize, 22)),
    ACSetEntitySource(InPort, BasicPort(PropMap())(es))
      .withProps(PropMap() + (MinimumWidth, 40))
      .addPropsBy((e: Entity, acs: ACSet, em: EntityMap) =>
        acs.props ++ style(acs, e.asInstanceOf[Part])
      ),
    ACSetEntitySource(OutPort, BasicPort(PropMap())(es))
      .withProps(PropMap() + (MinimumWidth, 40))
      .addPropsBy((e: Entity, acs: ACSet, em: EntityMap) =>
        acs.props ++ style(acs, e.asInstanceOf[Part])
      ),
    ACSetEntitySource(Wire, BasicWire(Src, Tgt)(es))
      .addPropsBy((ent, acs, emap) =>
        wireProps(Src, Tgt, style, portDir, es.bgPart)(ent, acs, emap)
      )
  )

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {

      // TODO: Figure out how to read a file on load
      // val import_str: String = howDoYouReadAFile("pie.json")

      val acsets = read[Map[String, ujson.Value]](import_str)

      implicit val rw: ReadWriter[(ACSet, Complex)] =
        SchDWD.runtimeSerializer("dims", es.size.now())

      val (acs, oldDims) = read[(ACSet, Complex)](acsets("pie_str"))

      for {
        g <- IO(UndoableVar(acs.scale(oldDims, es.size.now())))
        lg <- IO(
          es.size.signal
            .combineWith(g.signal)
            .map(DPBox.layoutPortsBg(InPort, OutPort))
        )
        vp <- es.makeViewport(
          es.MainViewport,
          lg,
          entitySources(es)
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui, vp))
      } yield ()
    }
  }
}
