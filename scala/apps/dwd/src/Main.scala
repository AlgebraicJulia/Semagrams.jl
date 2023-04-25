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
import semagrams.util.msgError
import semagrams.sprites.DPBox
import ujson.True


case object SchDWD extends Schema {

  val obs = Seq(DWDObs.values*)
  val homs = Seq(DWDHoms.values*)
  val attrs = Seq()



  enum DWDObs(_schema: Schema = SchEmpty) extends Ob:    
    override lazy val schema = _schema
    case InPort, OutPort, Wire
    case Box extends DWDObs(SchDWD)

  import DWDObs._


  enum DWDHoms(val doms:Seq[PartType],val codoms:Seq[PartType]) extends Hom:
    case Src extends DWDHoms(
      Wire.asDom(),
      InPort.asDom() :+ Box.extend(OutPort)
    )
    case Tgt extends DWDHoms(
      Wire.asDom(),
      OutPort.asDom() :+ Box.extend(InPort)
    )

  
  
}

import SchDWD._
import DWDObs._
import DWDHoms._




def bindings(
  es: EditorState, 
  g: UndoableVar[ACSet], 
  ui: UIState,
  vp: Viewport
) = {

  val a = Actions(es,g,ui)

  
  val boxMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
    ("Add Input", (b:Part) => IO(println("addIn"))),
  )

  val wireMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
  )



  def liftPort(p: Part, pos: Complex): Seq[(Ob,Int)] =
    val ext = p - es.bgPart
    val (sz,c) = ext.ty.path match
      case Seq() => (
        es.size.now(),
        es.size.now() / 2.0
      )
      case Seq(Box) => (
        a.getDims(p),
        g.now().trySubpart(Center,p).getOrElse(
          throw msgError(s"missing center for $p")
        )
      )
      case _ => 
        return Seq()

    val ptype = if pos.x < c.x then InPort else OutPort

    val nports = g.now().parts(p,ptype).length
    val j = DPBox.portNumber(pos - c + (sz/2.0),sz,nports)
    Seq((ptype,j))



  def test(p:Part) = es.mousePos.map(z => 
    println(s"hovered = $p")    
  )


  val layout = DPBox.layoutPortsBg(InPort,OutPort)
  val esources = entitySources(es)

  import MouseButton._
  import KeyModifier._


  Seq(
    // test hovered part
    keyDown("t").andThen(fromMaybe(es.hoveredPart).flatMap(test)),

    // Add box at mouse 
    dblClickOnPart(Left,ROOT.ty).withMods()
      .flatMap(_ => a.addAtMouse(Box)),

    // Edit box label
    dblClickOnPart(Left)
      .withMods()
      .map(b => es.bgPlus(b))
      .flatMap(a.edit(Content,true)),

    // Zoom into box
    dblClickOnPart(Left,PartType(Seq(Box)))
      .withMods(Ctrl)
      .map(b => es.bgPlus(b))
      .flatMap(b => a.zoomIn(b,layout,esources)),

    
    // Zoom out of box
    dblClickOn(Left).withMods(Ctrl).flatMap(_ => a.zoomOut(layout,esources)),

    // Drag box
    clickOnPart(Left).withMods()
      .flatMap(p => 
        p.ty match
        case ROOT.ty => es.mousePos.flatMap(
          z => IO(())
        )
        case PartType(Seq(Box,_*)) => a.dragMove(p.head)
      ),

    // Drag wire (or unplug)     
    clickOnPart(Left).withMods(Shift)
      .map(es.bgPlus(_))
      .flatMap(p =>
        val wires = (g.now().incident(p,Src) ++ g.now().incident(p,Tgt))
          .filter(_.ty == es.bgPart.ty.extend(Wire))

        if wires.isEmpty
        then 
          a.dragEdge(Wire,Src,Tgt,liftPort)(p)
        else 
          a.unplug(p,wires(0),Src,Tgt,liftPort)
      ),

    // Menu actions
    menuOnPart().flatMap(p => 
      p match
      case w if w.lastOb == Wire => 
        es.makeMenu(ui,wireMenu)(p)
      case b if b.lastOb == Box =>
        es.makeMenu(ui,boxMenu)(p)
      case _ => a.die
    ),

    // Delete part
    keyDown("d").andThen(a.del),

    // Undo
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),

    // Redo
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),

    // Print current state
    keyDown("?").andThen(a.debug),

    // Open serialization window
    keyDown("s").andThen(a.importExport)
  )
}



  


def portDir(p:Part) = p.ty.path match {
    case Seq(Box,OutPort) | Seq(InPort)
      => 10.0
    case Seq(Box,InPort) | Seq(OutPort)
      => -10.0
    case _ => 0.0
  }

val entitySources = (es:EditorState) => Seq(
  ACSetEntitySource(Box, 
  AltDPBox(InPort, OutPort, 
    PropMap() + (Fill,"lightblue"),           // InPort props
    PropMap() + (Fill,"lightgreen")           // OutPort props
  )(es)).withProps(PropMap() + (FontSize,22)),
  ACSetEntitySource(InPort, BasicPort()(es))
    .withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightblue")),
  ACSetEntitySource(OutPort, BasicPort()(es))
    .withProps(PropMap() + (MinimumWidth,40) + (Fill,"lightgreen")),
  ACSetEntitySource(Wire, BasicWire(es))
    .withProps(PropMap() + (Stroke,"black"))
    .addPropsBy(
      wireProps(Src, Tgt, portDir, es.bgPart)
    )
)






object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = ACSet(SchDWD)
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal)
            .map(DPBox.layoutPortsBg(InPort,OutPort))
        )
        vp <- es.makeViewport(
          "mainVP",
          lg,
          entitySources(es)
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui,vp))
      } yield ()
    }
  }
}
