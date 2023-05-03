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
import semagrams.PValue
import cats.Traverse


// import formula._

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


  enum DWDAttrs(val doms:Seq[PartType]) extends Attr:
    // case WireType extends DWDAttrs() with PValue[DWDType]
    case PortType extends DWDAttrs(
      Wire.asDom() ++ InPort.asDom() ++ OutPort.asDom()
        :+ Box.extend(InPort) :+ Box.extend(OutPort)
    ) with PValue[DWDType]

  enum DWDType derives ReadWriter:
    case Person, Document, Standard, Requirement

    def props = this match
      case Person      => PropMap() + (Fill,"red") + (Stroke,"red")
      case Document    => PropMap() + (Fill,"blue") + (Stroke,"blue")
      case Standard    => PropMap() + (Fill,"green") + (Stroke,"green")
      case Requirement => PropMap() + (Fill,"purple") + (Stroke,"purple")

    

  
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

  val a = Actions(es,g,ui)

  


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


  def setType(p:Part,tpe:DWDType): IO[Unit] = 
    p.lastOb match
    case Wire => (for
      _ <- a.set(p,PortType,tpe)
      s = g.now().trySubpart(Src,p)
      t = g.now().trySubpart(Tgt,p)
      _ <- s match
        case Some(p) if g.now().trySubpart(PortType,p) != Some(tpe) => setType(p,tpe)
        case _ => IO(())
      _ <- t match
        case Some(p) if g.now().trySubpart(PortType,p) != Some(tpe) => setType(p,tpe)
        case _ => IO(())
    yield ())
    case InPort | OutPort => (for 
      _ <- a.set(p,PortType,tpe)
      ws = (g.now().incident(p,Src) ++ g.now().incident(p,Tgt))
        .filter(w => g.now().trySubpart(PortType,w) != Some(tpe))
      _ <- a.doAll(ws.map(setType(_,tpe)))
    yield ())
  

  def test(p:Part) = es.mousePos.map(z => 
    val tpe = p.lastOb match
      case Wire => g.now().trySubpart(PortType,p)
      case InPort | OutPort => g.now().trySubpart(PortType,p)
    println(s"tpe = $tpe")
  )


  val layout = DPBox.layoutPortsBg(InPort,OutPort)
  val esources = entitySources(es)
  
  

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
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,true)(b))),
  )

  def menuItem(tpe:DWDType) = (
    s"Set type to $tpe",
    (p:Part) => setType(es.bgPlus(p),tpe)
  )

  val wireMenu: Seq[(String,Part => IO[Unit])] = DWDType.values.toSeq.map(menuItem)



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
        
        def eqTypes(p:Part,q:Part): IO[Unit] =
          println(s"eqTypes p = $p, q = $q")
          val ptpe = g.now().trySubpart(PortType,p)
          val qtpe = g.now().trySubpart(PortType,q)
          println(s"ptpe = $ptpe, qtpe = $qtpe")
          (ptpe,qtpe) match
            case (Some(tpe1),Some(tpe2)) => 
              if 
                tpe1 == tpe2
              then 
                println("case 1")
                IO(())
              else 
                println("case 2")
                a.die
            case (Some(tpe),None) => 
              println("case 3")
              setType(q,tpe)
            case (None,Some(tpe)) => 
              println("case 4")
              setType(p,tpe)
            case (None,None) => 
              println("case 5")
              IO(())
          
          
          // typeLens: (Part => Option[DWDType],(Part,Option[DWDType]) => IO[Unit]) = (
          // p => g.now().trySubpart(PortType,p),
          // (p,tpeOpt) => tpeOpt match
          //   case Some(tpe) => setType(p,tpe)
          //   case None => IO(())
          // )

        if wires.isEmpty
        then 
          a.dragEdge(Wire,Src,Tgt,liftPort,eqTypes)(p)
        else 
          a.unplug(p,wires(0),Src,Tgt,liftPort,eqTypes)
      ),

    // Menu actions
    clickOnPart(Right).flatMap(p => 
      p.lastOb match
      case Wire | InPort | OutPort => 
        es.makeMenu(ui,wireMenu)(p)
      case Box =>
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
    keyDown("s").andThen(a.importExport),

    // Help window
    // keyDown("?").andThen(showPopoverUntil(helpText, keyDown("?"))),

  )
}

 


def portDir(p:Part) = p.ty.path match {
    case Seq(Box,OutPort) | Seq(InPort)
      => 10.0
    case Seq(Box,InPort) | Seq(OutPort)
      => -10.0
    case _ => 0.0
  }

def style(acs:ACSet,p:Part): PropMap = (p.lastOb match
    case Wire => acs.props.get(PortType)
    case InPort | OutPort => acs.props.get(PortType)
    case _ => None
  ).map(_.props).getOrElse(PropMap())

val entitySources = (es:EditorState) => Seq(
  ACSetEntitySource(Box, 
  AltDPBox(InPort, OutPort,style)(es))
    .withProps(PropMap() + (FontSize,22)),
  ACSetEntitySource(InPort, BasicPort()(es))
    .withProps(PropMap() + (MinimumWidth,40))
    .addPropsBy((e:Entity,acs:ACSet,em:EntityMap) =>
      acs.props ++ style(acs,e.asInstanceOf[Part])
    ),
  ACSetEntitySource(OutPort, BasicPort()(es))
    .withProps(PropMap() + (MinimumWidth,40))
    .addPropsBy((e:Entity,acs:ACSet,em:EntityMap) => 
      acs.props ++ style(acs,e.asInstanceOf[Part])
    ),
  ACSetEntitySource(Wire, BasicWire(es))
    .addPropsBy(
      wireProps(Src, Tgt, style, portDir, es.bgPart)
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
        // _ = es.elt.ref.
        _ <- es.bindForever(bindings(es, g, ui,vp))
      } yield ()
    }
  }
}
