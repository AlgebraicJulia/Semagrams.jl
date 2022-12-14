package semagrams.mwe

import com.raquo.laminar.api.L.{_, given}
import org.scalajs.dom
import scala.scalajs.js.annotation.JSExportTopLevel

import semagrams._
import semagrams.acsets.{_, given}
import semagrams.actions._
import semagrams.controllers._
import semagrams.sprites.{Box as BoxSprite,Wire as WireSprite,_}
import semagrams.util._

import upickle.default._

// ================== Dataypes & Enums ======================


enum Shape(shape:String):
  case Rectangle extends Shape("rect")
  case Oval extends Shape("oval")

object Shape {
  implicit val rw: ReadWriter[Shape] = macroRW
}





enum PortType(porttype:String):
  case InPort extends PortType("in")
  case OutPort extends PortType("out")
  // case SymPort extends PortType("symmetric")

import PortType._
object PortType {
  implicit val rw: ReadWriter[PortType] = macroRW
}




enum Orientation(dir: Complex):
  case LeftToRight extends Orientation(Complex(1,0))
  case BottomToTop extends Orientation(Complex(0,1))
  case RightToLeft extends Orientation(Complex(-1,0))
  case TopToBottom extends Orientation(Complex(0,-1))
import Orientation._

object Orientation {
  implicit val rw: ReadWriter[Orientation] = macroRW
}



enum BoxBorder(val dir:Complex):
  case Top extends BoxBorder(Complex(0,-1))
  case Bottom extends BoxBorder(Complex(0,1))
  case Left extends BoxBorder(Complex(-1,0))
  case Right extends BoxBorder(Complex(1,0))
import BoxBorder._


enum PortProp[T: ReadWriter] extends Property:
  case Direction extends PortProp[Complex]

  type Value = T
  val rw = summon[ReadWriter[T]]
export PortProp._

// ================= Schemas ==================


// Internals (objects, arrows & attributes)
case object Box extends Ob

case object boxLabel extends AttrWithDom with PValue[String] {
  val dom = Box
}
case object boxShape extends AttrWithDom with PValue[Shape] {
  val dom = Box
}
case object boxStroke extends AttrWithDom with PValue[Double] {
  val dom = Box
}
case object boxColor extends AttrWithDom with PValue[String] {
  val dom = Box
}
case object boxStyle extends AttrWithDom with PValue[String] {
  val dom = Box
}


case object Wire extends Ob

case object wireLabel extends AttrWithDom with PValue[String] {
  val dom = Box
}
case object wireStyle extends AttrWithDom with PValue[String] {
  val dom = Wire
}
case object wireStroke extends AttrWithDom with PValue[Double] {
  val dom = Wire
}
case object wireColor extends AttrWithDom with PValue[String] {
  val dom = Wire
}
case object BoxPort extends Ob

case object portBox extends HomWithDom {
  val dom = BoxPort
  val codom = Box
}
case object portWireB extends HomWithDom {
  val dom = BoxPort
  val codom = Wire
}
case object portTypeB extends AttrWithDom with PValue[PortType] {
  val dom = BoxPort
}

case object DiagPort extends Ob

case object portWireD extends HomWithDom {
  val dom = DiagPort
  val codom = Wire
}

case object portTypeD extends AttrWithDom with PValue[PortType] {
  val dom = DiagPort
}

case object DiagramOptions extends Ob
case object diagOrientation extends AttrWithDom with PValue[Orientation] {
  val dom = DiagramOptions
}



// Externals (schema object & type)

case object StringSchemaOb extends StaticSchema {
  val schema = BasicSchema(
    Box,boxLabel,boxShape,boxStroke,boxColor,boxStyle,
    Wire,wireLabel,wireStyle,wireStroke,wireColor,
    BoxPort,portBox,portWireB,portTypeB,
    DiagPort,portWireD,portTypeD,
    DiagramOptions,diagOrientation
  )
}
type StringSchema = StringSchemaOb.type


// ========= Graphs (type alias + extensions) =============





type StringDiag = ACSet[StringSchema]
  extension (d:ACSet[StringSchema]) {
// case class StringDiag(d:ACSet[StringSchema]) {
    def boxes: Set[Part] = d.parts(Box)
    def wires: Set[Part] = d.parts(Wire)
    def diagPorts: Set[Part] = d.parts(DiagPort)
    def diagPorts(pt:PortType): Set[Part] = diagPorts.intersect(d.incident(portTypeD,pt))
    def boxPorts(b: Part): Set[Part] = d.incident(portBox,b)
    def boxPorts(b: Part,pt:PortType): Set[Part] = boxPorts(b).intersect(
      d.incident(portTypeB,pt)
    )
    def boxPorts(): Set[Part] = boxes.flatMap(boxPorts(_))
    def wirePorts(w: Part): Set[Part] = d.incident(portWireD,w).union(d.incident(portWireB,w))

    def ports: Set[Part] = boxPorts().union(diagPorts)

  
    def portWire(p: Part): Option[Part] = p.ob match
      case BoxPort => d.trySubpart(portWireB,p)
      case DiagPort => d.trySubpart(portWireD,p)


    def endPts(p: Part): Set[Part] = portWire(p) match
      case Some(w) => wirePorts(w) - p
      case None => Set()
  
    def endPt(p: Part): Option[Part] = endPts(p).toList match
      case Nil => None
      case x::y => Some(x)

    def orientation: Orientation = (for
      opt <- d.parts(DiagramOptions).toSeq
      o <- d.trySubpart(diagOrientation,opt) 
    yield o) match
      case Seq(x,_ @_*) => x
      case Seq() => TopToBottom
    
    
        
}

object StringDiag {
  def apply(orientation: Orientation=Orientation.TopToBottom) = 
    val (opt,_) = ACSet[StringSchema]().addPart(DiagramOptions,PropMap().set(diagOrientation,orientation))
    opt
}


def buildDiag() =
  val (d1,b1) = StringDiag().addPart(Box,PropMap().set(Center,Complex(100,100)))
  val (d2,b2) = d1.addPart(Box,PropMap().set(Center,Complex(200,200)))
  val (d3,w) = d2.addPart(Wire)
  val (d4,p1) = d3.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w).set(portTypeB,OutPort))
  val (d5,p2) = d4.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w).set(portTypeB,InPort))
  val (d6,w2) = d5.addPart(Wire)
  val (d7,p3) = d6.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w2).set(portTypeB,OutPort))
  val (d8,p4) = d7.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w2).set(portTypeB,OutPort))
  d8
  // val (d9,w3) = d8.addPart(Wire)
  // val (d10,p5) = d9.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w3).set(portTypeB,InPort))
  // val (d11,p6) = d10.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w3).set(portTypeB,InPort))

  // val (d12,w4) = d11.addPart(Wire)
  // val (d13,p7) = d12.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w4).set(portTypeB,InPort))
  // val (d14,p8) = d13.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w4).set(portTypeB,OutPort))

  // val (d15,w5) = d14.addPart(Wire)
  // val (d16,p9) = d15.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w5).set(portTypeB,OutPort))
  // val (d17,p10) = d16.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w5).set(portTypeB,InPort))

  // d17

//================== Interaction ========================

// Actions

val ops = Action.ops[StringDiag]
val aops = summon[ACSetOps[StringSchema]]



def addBox(props:PropMap = PropMap()): Action[StringDiag,Part] = for
  p <- mousePos
  b <- ops.ask.flatMap(_ => addPart(Box,
    PropMap().set(Center,p)
  ))
yield b

def addWire(props:PropMap = PropMap()): Action[StringDiag,Part] = ops.ask.flatMap(_ => addPart(Wire,props))

// def addPort(b: Entity,w: Wire,props:PropMap = PropMap()) = for
// def addPort(b:Entity,w:Entity,props:PropMap = PropMap()): Action[StringDiag,Part] = addPart(BoxPort, 
//   props.set(portWireB,w.asInstanceOf[Part])
//   .set(portBox,b.asInstanceOf[Part]))

// def addWire(b1:Entity,b2:Entity,props:PropMap): Action[StringDiag,(Part,Part,Part)] = for
//   w <- ops.ask.flatMap((_ => addPart(Wire,props)))
//   p1 <- addPort(b1,w)
//   p2 <- addPort(b2,w)
// yield (w,p1,p2)

// def addWire(b1:Entity,props:PropMap = PropMap()): Action[StringDiag,(Part,Part)] = for
//   w <- ops.ask.flatMap((_ => addPart(Wire,props)))
//   p1 <- addPort(b1,w)
// yield (w,p1)


def initialize(d:StringDiag): Action[StringDiag,Unit] = for
  _ <- ops.ask.flatMap(_ => updateModel(_ => d))
  // ns <- ops.ask.map(_.$model.now().parts(Box).toSeq)
  // zs <- randPts(ns.length)
  // _ <- ops.ask.flatMap(
  //   _ => setSubparts(Center,ns.zip(zs))
  // )
yield ()
  


// def dragString(box1:Entity): Action[StringDiag,Part] = for
//   p <- mousePos
//   w
  
//   drag <- ops.ask.map(_.drag)
//   $m <- ops.ask.map(_.$model)
//   wp <- addWire(box1,PropMap().set(Start,p).set(End,p))
//   (wire,port) = wp
//   _ <- (for 
//     _ <- drag.drag(
//       Observer(p => $m.update(
//         _.setSubpart(End,wire,p)
//       ))
//     )
//     box2 <- fromMaybe(hoveredPart(Box))
//     port2 <- addPort(box2,wire)
//     _ <- updateModelS(
//       aops.remSubpart(End,wire)
//     )
//     _ <- updateModelS(
//       aops.remSubpart(Start,wire)
//     )
//   yield ()).onCancelOrError(for
//     _ <- ops.delay(drag.$state.set(None))
//     _ <- updateModelS(aops.remPart(wire))
//   yield ())
//   _ <- update
// yield wire


// def setString(strAttr: PValue[String],n: Entity): Action[StringDiag,Unit] = editStringProp(strAttr)(n.asInstanceOf[Part])



def test(b:Entity): Action[StringDiag,Unit] = for
  p <- ops.ask.flatMap(_ => mousePos)
  b <- ops.ask.flatMap(_ => addPart(BoxPort,
    PropMap().set(Center,p)
  ))
yield ()


def logModel: Action[StringDiag,Unit] = for
  m <- ops.ask.map(_.$model.now())
  _ <- log(m.parts)
  _ <- log(m.props)
yield ()


// Bindings



val bindings = Bindings[StringDiag, Unit](
  clickOn(ClickType.Double,MouseButton.Left,BackgroundType)
    .flatMap(_ => addBox())
    .map(_ => ()),
  clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
    .flatMap(_ => dragPan),
  clickOn(ClickType.Single,MouseButton.Right,BackgroundType)
    .flatMap(_ => logModel),

  clickOn(ClickType.Single,MouseButton.Left,Box)
    .flatMap(b => dragPart(b.asInstanceOf[Part])),
  clickOn(ClickType.Double,MouseButton.Left,Box)
    .flatMap(b => test(b)),
//   clickOn(ClickType.Single,MouseButton.Left,Box)
//     .withMods(KeyModifier.Shift)
//     .flatMap(box1 => dragString(box1))
//     .map(_ => ()),

  keyDown("Delete").andThen(remPart)
)



// =========== Rendering =======================

// Entity & Prop extraction

def extractBoxes(d: StringDiag): List[(Part,PropMap)] = d.boxes.map(b => 
  val o = d.orientation
  val ps = d.props(b)
  val label = ps.get(boxLabel).getOrElse(o.toString())
  (b, ps.set(Content,label))
).toList





def extractBoxPorts(d: StringDiag,sprites: Sprites): List[(Part,PropMap)] = d.boxes.toList.flatMap(b =>
    val (spr,props) = sprites(b)
    val (_,dims) = BoxSprite().geom(props)
    val c: Complex = props(Center)
    val o = d.orientation

    d.boxPorts(b).toList.zipWithIndex.map((p,k) =>
      val (ptype,slot) = portSlot(d,p)
      val n_ports = d.boxPorts(b,ptype).size
      val brdr = border(o,ptype)
      val ppos = c + portDelta(brdr,dims,slot,n_ports)

      (p,PropMap()
        .set(Center,ppos)
        .set(Content,"")
        .set(Direction,brdr.dir)
      )
    )
)


// def extractDiagPorts(d: StringDiag,sprites: Sprites): List[(Part,PropMap)] = 
//   val (spr,props) = sprites(Background)
//   // val c: Complex = props(Center)
//   val o = d.orientation

//   val boxports = d.boxPorts(b).toList.zipWithIndex.map((p,k) =>
//     val (ptype,slot) = portSlot(d,p)
//     val n_ports = d.boxPorts(b,ptype).size
//     val brdr = border(o,ptype)
//     val ppos = c + portDelta(brdr,dims,slot,n_ports)

//     (p,PropMap().set(Center,ppos))
// )


def border(o: Orientation,pt: PortType) = (o,pt) match
  case (LeftToRight,InPort) | (RightToLeft,OutPort) => Left
  case (LeftToRight,OutPort) | (RightToLeft,InPort) => Right
  case (TopToBottom,InPort) | (BottomToTop,OutPort) => Top
  case (TopToBottom,OutPort) | (BottomToTop,InPort) => Bottom


def portDelta(brdr: BoxBorder, dims: Complex,slot: Int,n_ports: Int) = 
  val frac = -.5 + (.5 + slot)/n_ports
  brdr match
    case Top => Complex(frac * dims.x, -dims.y/2)
    case Bottom => Complex(frac * dims.x, dims.y/2)
    case Left => Complex(-dims.x/2,frac * dims.y)
    case Right => Complex(dims.x/2,frac * dims.y)




def portSlot(d: StringDiag, p: Entity): (PortType,Int) =
  val porttype: PortType = d.trySubpart(portTypeB,p.asInstanceOf[Part]).getOrElse(OutPort)
  val box: Option[Part] = d.trySubpart(portBox,p.asInstanceOf[Part])

  val slots = box match
    case Some(b) => d.boxPorts(b,porttype).toSeq
    case None => d.diagPorts(porttype).toSeq

  (porttype,slots.indexOf(p))


def extractWires(d: StringDiag,sprites:Sprites) = d.wires.toList.map(w =>
  d.wirePorts(w).toSeq match
    case Seq(s,t,u @_*) => 
      if u.length > 0 
        then println("Warning: extra wire ports: " + d.wirePorts(w))
      (w,wireProps(d,s,t,sprites))
    case _ => (w,dragProps(d,w,sprites))

)

def dragProps(d: StringDiag,w: Part,sprites: Sprites) = {
  val zs = d.trySubpart(Start,w).getOrElse(Complex(0,0))
  val zt = d.trySubpart(End,w).getOrElse(Complex(10,10))
  val dir = if zs == zt then Complex(1,0) else (zt-zs).normalize
  PropMap()
    .set(Start,zs)
    .set(End,zt - 10*dir)
    .set(Bend,0)
}


def wireProps(d: StringDiag,s: Part, t: Part, sprites: Sprites) = 
  val (sspr,sprops) = sprites(s)
  val (tspr,tprops) = sprites(t)

  val bend = 100
  

  PropMap()
    .set(Start,sprops(Center))
    .set(StartDir,sprops(Direction))
    .set(End,tprops(Center))
    .set(EndDir,tprops(Direction))
    .set(Bend,bend)

    // .set(Start,zs)
    // .set(StartDir,dzs)
    // .set(End,zt)
    // .set(EndDir,dzt)
    // .set(Bend,d.props(w).get(Bend).getOrElse(0))

  // val pts = for
  //   s <- d.trySubpart(src,e)
  //   t <- d.trySubpart(tgt,e)
  // yield (s,t)
  // pts match
  //   case Some((s,t)) => 
  //     if s == t 
  //     then (e,loopProps(d,e,sprites))
  //     else (e,edgeProps(d,e,sprites))
  //   case None => (e,dragProps(d,e,sprites))


// def loopProps(d: StringDiag,e: Part,sprites: Sprites) = {
//   val s: Part = d.subpart(src,e)
//   val (spr: Sprite,sprops: PropMap) = sprites(s)
//   val c: Complex = sprops.get(Center).getOrElse(Complex(0,0))
//   val dir: Complex = Complex(0,1)

//   val slot: Int = getSlot(d,e)
//   val bend: Double = .1 + .05 * slot
//   val rot: Complex = Complex(0,bend).exp

//   val bs: Complex = spr.boundaryPt(sprops,-dir*rot)
//   val bt: Complex = spr.boundaryPt(sprops,-dir*rot.cong)

//   PropMap()
//     .set(Loop,c)
//     .set(Start,bs)
//     .set(End,bt)
//     .set(Bend,bend)
// }

// def edgeProps(d: StringDiag,e: Part,sprites: Sprites) = {
//   val s: Part = d.subpart(src,e)
//   val t: Part = d.subpart(tgt,e)
//   val (sspr,sprops) = sprites(s)
//   val (tspr,tprops) = sprites(t)
//   val cs = sprops.get(Center).getOrElse(Complex(0,0))
//   val ct = tprops.get(Center).getOrElse(Complex(10,10))
//   val dir = if ct == cs 
//     then Complex(1,0)
//     else (ct - cs).normalize

  
//   val slot = getSlot(d,e)
//   val bend = .2 * slot
//   val rot = Complex(0,bend).exp


//   val bs = sspr.boundaryPt(sprops,dir*rot)
//   val bt = tspr.boundaryPt(tprops,-dir*rot.cong)

//   PropMap()
//     .set(Start,bs)
//     .set(End,bt)
//     .set(Bend,bend)
// }

// def getSlot(d:StringDiag,e: Part) = 
//   (d.srcOf(e),d.tgtOf(e)) match
//     case (Some(s),Some(t)) =>
//       val idx = d.edgesFrom(s,t).toList.indexOf(e)
//       val fwd = d.edgesFrom(s,t).size
//       val bwd = d.edgesFrom(t,s).size
//       if bwd == 0 then
//         (1 - fwd) + 2 * idx
//       else
//         1 + 2 * idx    
//     case _ => 0


  // Middleware (controllers & defaults)
def nodeMiddleware(
  hover:HoverController,
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() 
    + (Stroke,"black")
    + (Fill, "lightgray")
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)
    + (InnerSep, 7)
    + (FontSize, 16)
    + (Style, "margin:50px;")
  ),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(0.7)")
  ),
  Clickable(mouse, MainHandle)
)

def boxPortMiddleware(
  hover:HoverController,
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() 
    + (Stroke,"none")
    + (Fill, "red")
    + (MinimumWidth, 10)
    + (MinimumHeight, 10)
    + (InnerSep, 5)
    + (FontSize, 16)
    + (Style, "filter: opacity(0.7)")
  ),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(1)")
  ),
  Clickable(mouse, MainHandle)
)

def wireMiddleware(
  hover:HoverController, 
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() + (Stroke,"black")),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(0.7)")
  ),
  Clickable(mouse, MainHandle)
)

// Sprite makers


def boxMaker(
  hover:HoverController,
  mouse:MouseController
) = SpriteMaker[StringDiag](
  BoxSprite(),
  (d:StringDiag,spr) => extractBoxes(d),
  nodeMiddleware(hover,mouse)
)

def portBoxMaker(
  hover:HoverController,
  mouse:MouseController
) = SpriteMaker[StringDiag](
  Disc(),
  (d:StringDiag,spr) => extractBoxPorts(d,spr),
  boxPortMiddleware(hover,mouse)
)

def wireMaker(hover:HoverController,mouse:MouseController) = SpriteMaker[StringDiag](
  WireSprite(),
  extractWires,
  wireMiddleware(hover,mouse)
)


// // DOM object (svg)

def renderApp(
  $appModel: Var[StringDiag],
  hover: HoverController,
  mouse: MouseController
) = {
  val spriteMaps = SpriteMaps[StringDiag](
    $appModel.signal,                     // : Signal[StringDiag]
    List(
      boxMaker(hover,mouse),
      portBoxMaker(hover,mouse),
      wireMaker(hover,mouse),
    )
  )
  
  svg.g(
    spriteMaps.attach
  )
}


//================ Main =====================

val serializer = ACSet.rw[StringSchema]

object Main {

  // val d: StringDiag = buildGraph()

  

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- initialize(buildDiag())
      _ <- addRelative(renderApp($model,hover,mouse))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head")
      .appendChild(styleTag().ref)

    plutoMain[StringDiag](el,StringDiag(),serializer,action)
  }
}








// Schema elements




// Schema/Instance types


// Actions & Bindings



// def remLabel(n: Part): Action[StringDiag,Unit] = for
//   pm <- ops.ask.map(_.$model.now().props(n))
//   _ <- updateModel(
//     (d: StringDiag) => d.remSubpart(label,n)
//   )
// yield ()


// def getProps(x: Part): Action[StringDiag,PropMap] = ops.ask.map(_.$model.now().props(x))























