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
import semagrams.sprites.{AltDPBox,BasicPort}

case object Box extends Ob {
  override val schema = SchDWD
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
  def apply(init:ACSet) = new ACSet(SchDWD,init.props,init.partsMap)
}










def bindings(
  es: EditorState, 
  g: UndoableVar[ACSet], 
  ui: UIState,
  vp: Viewport
) = {

  

  def dwdFromJson(s:String): Option[ACSet] = Some(DWD())

  def jsonFromDWD(dwd: ACSet): String = g.now().toString()



  val a = Actions(es, g, ui,jsonFromDWD,dwdFromJson)

  val boxMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
    ("Add Input", (b:Part) => IO(println("addIn"))),
  )

  val wireMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
  )


  def portNumber(pos:Complex,size:Complex,nports:Int) =
    val l = (0 to nports+1).map(
      _ * size.y/(nports + 1)
    )
    // println(s"l = $l")
    // println(s"pos = $pos")
    val hit = l.lastIndexWhere(_ < pos.y)
    // println(l.map(_ < pos.y))
    // println(hit)
    hit


  import MouseButton._
  import KeyModifier._

  def portByPos(p:Part,pos:Complex): Option[(Ob,Int)] = p match
    case b if b == es.bgPart() =>
      // println(s"portByPos bg $b")
      val sz = es.size.now()
      val ptype = if pos.x < (sz.x / 2.0)
        then InPort
        else OutPort
      val nports = g.now().subacset(b).partsMap(ptype).ids.length
      Some((ptype,portNumber(pos,sz,nports)))
    case p if Seq(InPort,OutPort).contains(p.ty.path.last) => None 
    case b if b.init == es.bgPart() =>
      println(s"portByPos box $b")
      val ctr = g.now().subacset(es.bgPart()).trySubpart(Center,p-es.bgPart()).getOrElse({
        println("missing center")
        throw new RuntimeException("broke")
      })
      val ptype = if pos.x < ctr.x then InPort else OutPort
      val nports = g.now().subacset(b).partsMap(ptype).ids.length
      // val sz = a.getSize(b)
      // println(s"sz = $sz")
      // println(s"x: ${ctr.x > pos.x}")
      // println(s"y: ${ctr.y > pos.y}")
      Some((ptype,nports))
    case _ => 
      // println(s"unknown portByPos $p")
      None
  

  // def boxSize(b:Part): Option[Complex] = 
  


  def zoomIn(b:Part) =  
    // println(s"zoomIn $b")
    es.hover.$state.set(HoverController.State(None))
    es.currentView.set(b)
    val vpold = es.viewports.now().toSeq(0)
    es.deregister(vpold)
    for 
      // bb <- a.getBBox(b)
      // _ = println(bb)
      vp <- es.makeViewport(
        es.size.signal.combineWith(g.signal.map(_.subacset(b)))
          .map(layoutPorts),
        entitySources(es)
      ).map(vp => 
        es.viewports.set(Set(vp))
      )
    yield vp

  def zoomOut = 
    val b = es.bgPart() match
      case ROOT => ROOT
      case p => p.init
    zoomIn(ROOT).flatMap(_ => zoomIn(b))


  
  Seq(
    keyDown("t").andThen(
        for
        p <- fromMaybe(es.hoveredPart)
        z <- es.mousePos
        _ = println(es.entities.now().em.keys)
        // _ = println(portByPos(p,z))
      yield ()
    ),
    // clickOn(Left).map(ent => println(s"click ent = $ent")),
    // clickOnPart(Left).map(pt => println(s"click pt = $pt")),
    // Add box
    dblClickOnPart(Left,ROOT.ty).withMods().flatMap(
      _ => for
        b <- a.addAtMouse(Box)
        // _ <- a.set(b,Content,s"${util.Random.alphanumeric.take(3).mkString}")
        _ <- a.edit(Content,true)(b)
      yield ()
    ),
    dblClickOnPart(Left,PartType(Seq(Box)))
      .withMods()
      .map(b => es.bgPlus(b))
      .flatMap(zoomIn),

    dblClickOn(Left).withMods(Ctrl).flatMap(_ => zoomOut),
    clickOnPart(Left).withMods(Alt)
      .flatMap(p => 
        p.ty match
        case ROOT.ty => es.mousePos.flatMap(
          z => IO(())//IO(println(s"clicked at $z"))
        )
        case PartType(Seq(Box,_*)) => a.dragMove(p.head)
      ),
    clickOnPart(Left).withMods(Shift)
      .map(es.bgPlus(_))
      .flatMap(p => 
        a.dragEdge(Wire,Src,Tgt,portByPos)(p)
      ),
    menuOnPart().flatMap(p => 
      println(s"menuOnPart $p")
      p match
      case w if w.lastOb == Wire => 
        es.makeMenu(ui,wireMenu)(p)
      case b if b.lastOb == Box =>
        es.makeMenu(ui,boxMenu)(p)
      case _ => a.die
    ),
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
      {
        case ((p, sub), i) => (p, sideCenter + spacer.assignPos(i,n) * im)
      }
    )
    cs.foldLeft(acs)((acs, pc) => acs.setSubpart(pc._1, Center, pc._2))
  }
  helper(helper(init, InPort, -1), OutPort, 1)
}






def wireProps(
    src: Hom,
    tgt: Hom,
    ext: Part => Part
)(_e: Entity, acs: ACSet, m: EntityMap): PropMap = {
  val p = acs.props
  val s = p.get(src)
  val t = p.get(tgt)

  // println(s"""wireProps
  //   src = $src
  //   tgt = $tgt
  //   s = $s
  //   t = $t
  //   _e = $_e
  //   acs = $acs
  //   ext(s) = ${s match 
  //     case Some(p) => ext(p)
  //     case None => "None"
  //   }
  //   """)

  val tspos = s.flatMap(p => findCenter(ext(p),m))
  val ttpos = t.flatMap(p => findCenter(ext(p),m))
  
  val spos = tspos match
    case Some(z) => z
    case None => 
      // println(s"no spos")
      p.get(Start).getOrElse(Complex(100,100))

  val tpos = ttpos match
    case Some(z) => z
    case None => 
      // println(s"no tpos")
      p.get(End).getOrElse(Complex(100,100))

  
  def getDir(s:Option[Part]) = s match
    case Some(p) 
      if p.ty.path == Seq(Box,OutPort) |
        p.ty.path == Seq(InPort)
      => 10.0
    case Some(p) 
      if p.ty.path == Seq(Box,InPort) |
        p.ty.path == Seq(OutPort)
      => -10.0
    case _ => 0.0

  val (startDir,endDir) = (getDir(s),getDir(t))

  PropMap().set(WireProp.StartDir,startDir).set(WireProp.EndDir,endDir)
    + (Start, spos) + (End, tpos) + (Stroke,"black")
    
}



def ACSetWireSource(
    ob: Ob,
    src: Hom,
    tgt: Hom,
    sprite: Sprite,
    ext: Part => Part
) = ACSetEntitySource(ob, sprite).addPropsBy(wireProps(src, tgt, ext))

val entitySources = (es:EditorState) => Seq(
  ACSetEntitySource(Box, AltDPBox(InPort, OutPort)(es)).withProps(PropMap() + (FontSize,22)),
  ACSetEntitySource(InPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightblue")),
  ACSetEntitySource(OutPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightblue")),
  ACSetWireSource(Wire, Src, Tgt, BasicWire(es),p => p),
)






object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      // val initg = DWD()
      val initg = proc
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal)
            .map(layoutPorts)                        
        )
        vp <- es.makeViewport(
          lg,
          entitySources(es)
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui,vp))
      } yield ()
    }
  }
}
