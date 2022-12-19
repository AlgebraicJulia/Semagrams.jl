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
import Complex.{one,im}


import upickle.default._

// ================== Dataypes & Enums ======================


import PortType._
import Orientation._
import BoxBorder._



enum PortType(porttype:String):
  case InPort extends PortType("in")
  case OutPort extends PortType("out")

  def rev = this match
    case InPort => OutPort
    case OutPort => InPort
object PortType {
  implicit val rw: ReadWriter[PortType] = macroRW
}

enum Orientation(val dir: Complex):
  case BottomToTop extends Orientation(-im)
  case TopToBottom extends Orientation(im)
  case LeftToRight extends Orientation(one)
  case RightToLeft extends Orientation(-one)

  def rev = this match
    case BottomToTop => TopToBottom
    case TopToBottom => BottomToTop
    case LeftToRight => RightToLeft
    case RightToLeft => LeftToRight

  def inBorder = this match
    case BottomToTop => Bottom
    case TopToBottom => Top
    case LeftToRight => Left
    case RightToLeft => Right

  def outBorder = inBorder.rev


  // Get port information z = center of box/diagram 
  def getPortType(z: Complex) = if dir.dot(z) >= 0
    then OutPort
    else InPort
  
  def getPortSlot(z:Complex,nports:Int,dims: Complex): Int =
    // val pdist = z.dot()
    // val start = -dims.dot(im*dir)/2
    // val incr = start.abs/nports
    // val slot = (for
    //   i <- 0 to nports-1
    //   spot = start + (1 + 2*i)*incr
    //   if z.dot(im*dir) < spot
    // yield i)
    // slot.toSeq match
    //   case Seq() => nports
    //   case Seq(first,_*) => first
    nports    
    

object Orientation {
  implicit val rw: ReadWriter[Orientation] = macroRW
}



enum BoxBorder(val dir:Complex):
  case Top extends BoxBorder(-im)
  case Bottom extends BoxBorder(im)
  case Left extends BoxBorder(-one)
  case Right extends BoxBorder(one)

  def rev = this match
    case Top => Bottom
    case Bottom => Top
    case Left => Right
    case Right => Left




enum PortProp[T: ReadWriter] extends Property:
  case Direction extends PortProp[Complex]
  case PortIndex extends PortProp[Int]

  type Value = T
  val rw = summon[ReadWriter[T]]
import PortProp._

// ================= Schemas ==================


// Internals (objects, arrows & attributes)
case object Box extends Ob

case object boxLabel extends AttrWithDom with PValue[String] {
  val dom = Box
}
case object Wire extends Ob

case object wireLabel extends AttrWithDom with PValue[String] {
  val dom = Box
}

case object Port extends Ob

// No Box = global/diagram input/output
case object portBox extends HomWithDom {
  val dom = Port
  val codom = Box
}
case object portIndex extends AttrWithDom with PValue[Int] {
  val dom = Box
}
case object portWire extends HomWithDom {
  val dom = Port
  val codom = Wire
}
case object ptType extends AttrWithDom with PValue[PortType] {
  val dom = Port
}

case object DiagramOptions extends Ob
case object diagOrientation extends AttrWithDom with PValue[Orientation] {
  val dom = DiagramOptions
}



// Externals (schema object & type)

case object StringSchemaOb extends StaticSchema {
  val schema = BasicSchema(
    Box,boxLabel,Wire,wireLabel,
    Port,portBox,portWire,ptType,portIndex,
    DiagramOptions,diagOrientation
  )
}
type StringSchema = StringSchemaOb.type


// ========= Graphs (type alias + extensions) =============





type DWD = ACSet[StringSchema]
  extension (d:ACSet[StringSchema]) {
// case class DWD(d:ACSet[StringSchema]) {
    def boxes: Seq[Part] = d.parts(Box).toSeq
    def wires: Seq[Part] = d.parts(Wire).toSeq
    def ports: Seq[Part] = d.parts(Port).toSeq
    def boxOf(p: Part): Option[Part] = d.trySubpart(portBox,p)
    def wireOf(p: Part): Option[Part] = d.trySubpart(portWire,p)
    def indexOf(p: Part): Int = d.trySubpart(portIndex,p).getOrElse(
      ports.indexOf(p)
    )
    def typeOf(p: Part): PortType = d.trySubpart(ptType,p).getOrElse(OutPort)

    def portsOf(e: Entity): Seq[Part] = e.entityType match
      case w @ Wire => ports.filter(p => wireOf(p) == Some(e))
      case b @ Box => ports.filter(p => boxOf(p) == Some(e))
      case BackgroundType => ports.filter(p => boxOf(p) == None)
      case _ => Seq()
    
    def portsOf(e: Entity,pt:PortType): Seq[Part] = portsOf(e).filter(typeOf(_) == pt)
    
    def endPt(p: Part): Option[Part] = wireOf(p).flatMap(w => 
      portsOf(w).filterNot(_==p).headOption
    )
         
    def orientation: Orientation = (for
      opt <- d.parts(DiagramOptions).toSeq
      o <- d.trySubpart(diagOrientation,opt) 
    yield o) match
      case Seq(x,_ @_*) => x
      case Seq() => TopToBottom
    
    
        
}

object DWD {
  def apply(orientation: Orientation=Orientation.TopToBottom) = 
    val (opt,_) = ACSet[StringSchema]().addPart(DiagramOptions,PropMap().set(diagOrientation,orientation))
    opt
}


def buildDiag() =
  val (d1,b1) = DWD().addPart(Box,PropMap().set(Center,Complex(300,200)))
  val (d2,b2) = d1.addPart(Box,PropMap().set(Center,Complex(200,200)))
  val (d3,w) = d2.addPart(Wire)
  val (d4,p1) = d3.addPart(Port,PropMap().set(portBox,b1).set(portWire,w).set(ptType,OutPort))
  val (d5,p2) = d4.addPart(Port,PropMap().set(portWire,w).set(ptType,InPort))
  // val (d6,w2) = d5.addPart(Wire)
  // val (d7,p3) = d6.addPart(BoxPort,PropMap().set(portBox,b1).set(portWireB,w2).set(portTypeB,OutPort))
  // val (d8,p4) = d7.addPart(BoxPort,PropMap().set(portBox,b2).set(portWireB,w2).set(portTypeB,OutPort))
  d5
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

val ops = Action.ops[DWD]
val aops = summon[ACSetOps[StringSchema]]


// Adding

def addBox(props:PropMap = PropMap()): Action[DWD,Part] = for
  p <- mousePos
  b <- ops.ask.flatMap(_ => addPart(Box,
    PropMap().set(Center,p)
  ))
yield b

def addWire(p: Part,props:PropMap = PropMap()): Action[DWD,Part] = for
  w <- ops.ask.flatMap(_ => addPart(Wire,props))
  _ <- ops.ask.map(_ => setSubpart(portWire,p,w))
yield w



def addPort(b:Entity,pt:PortType,baseprops:PropMap = PropMap()): Action[DWD,Part] = 
  val props = b.entityType match
    case Box => baseprops.set(portBox,b.asInstanceOf[Part]).set(ptType,pt)
    case _ => baseprops.set(ptType,pt)
  addPart(Port,props)

def plug(w: Part,p: Part): Action[DWD,Unit] = setSubpart(portWire,p,w)
def unplug(w: Part,p: Part): Action[DWD,Unit] = for
  wp <- ops.ask.map(_.$model.now().trySubpart(portWire,p))
  _ <- wp == Some(w) match
    case true => updateModelS(
      aops.remSubpart(portWire,p)
    )
    case false => ops.pure(())
yield ()

def plug(wOpt: Option[Part],p: Part): Action[DWD,Unit] = wOpt match
  case Some(w) => plug(w,p)
  case None => ops.pure(())
def unplug(wOpt: Option[Part],p: Part): Action[DWD,Unit] = wOpt match
  case Some(w) => unplug(w,p)
  case None => ops.pure(())


// Deleting

def remPort(p: Part): Action[DWD, Unit] = ops.ask.map(_.$model.now().trySubpart(portBox,p))
  .flatMap(_ match
    case None => remDiagPort(p)
    case Some(b) => remBoxPort(p)
  )


def remBoxPort(p: Part): Action[DWD,Unit] = for
  m <- ops.ask.map(_.$model.now())
  w = m.wireOf(p)
  pt = m.typeOf(p)
  _ <- unplug(w,p)
  p2 <- addPort(Background,pt.rev)
  _ <- ops.ask.map(_ => plug(w,p2))
  _ <- ops.ask.flatMap(_ =>
    updateModel(_.remPart(p))  
  )
yield ()

def remDiagPort(p: Part): Action[DWD,Unit] = for
  m <- ops.ask.map(_.$model.now())
  p2Opt = m.endPt(p)
  wOpt = m.wireOf(p)
  _ <- p2Opt match 
    case Some(p2) => unplug(wOpt,p2)
    case None => ops.pure(())
  _ <- wOpt match
    case Some(w) => ops.ask.flatMap(_ =>
      updateModel(_.remPart(w))
    )
    case None => ops.pure(())
yield ()

def remWire(w: Part): Action[DWD,Unit] = for
  m <- ops.ask.map(_.$model.now())
  ps = m.portsOf(w)
  _ <- ops.ask.flatMap(_ => doAll(ps.map(p =>
    unplug(w,p)  
  )))
  _ <- ops.ask.flatMap(_ =>
    updateModel(_.remPart(w))
  )
yield ()

def remBox(b: Part): Action[DWD,Unit] = for
  m <- ops.ask.map(_.$model.now())
  ps = m.portsOf(b)
  _ <- doAll(ps.map(remBoxPort(_)))
  _ <- ops.ask.flatMap(_ =>
    updateModel(_.remPart(b))
  )
yield ()








// def scootPortsB(b: Part,pt:PortType,slot:Int) : Action[DWD,Unit] = for
//   m <- ops.ask.map(_.$model.now())
//   ports = m.portsOf(b,pt).toSeq
//   port_idxs = ports.zipWithIndex.map(
//     (p,k) => m.trySubpart(portIndexB,p).getOrElse(ports.length+k)
//   )
//   new_idxs = port_idxs.map(i => if slot > i then i else i+1)
//   _ <- ops.ask.map(_ => setSubparts(portIndexB,ports.zip(new_idxs)))
// yield ()
   
  


// def addBoxPort(b:Entity,w:Entity,pt:PortType,slot:Int,props:PropMap = PropMap()): Action[DWD,Part] = for
//   _ <- scootPortsB(b.asInstanceOf[Part],pt,slot)
//   p <- ops.ask.flatMap(_ => addPart(BoxPort, 
//     props.set(portWireB,w.asInstanceOf[Part])
//       .set(portBox,b.asInstanceOf[Part])
//       .set(portTypeB,pt)
//       .set(portIndexB,slot)
//   ))
// yield p




// def addWire(b1:Entity,b2:Entity,props:PropMap): Action[DWD,(Part,Part,Part)] = for
//   w <- ops.ask.flatMap((_ => addPart(Wire,props)))
//   p1 <- addPort(b1,w)
//   p2 <- addPort(b2,w)
// yield (w,p1,p2)

// def addWire(b1:Entity,props:PropMap = PropMap()): Action[DWD,(Part,Part)] = for
//   w <- ops.ask.flatMap((_ => addPart(Wire,props)))
//   p1 <- addPort(b1,w)
// yield (w,p1)


def getBoxPortType(b: Part): Action[DWD,PortType] = for
  z <- mousePos
  m <- ops.ask.map(_.$model.now())
yield
  val c = m.props(b)(Center)
  m.orientation.getPortType(z - c)


def getBoxPortSlot(b: Entity,pt:PortType) = for
  z <- mousePos
  m <- ops.ask.map(_.$model.now())
yield
  val nports = m.portsOf(b,pt).size
  val (c,dims) = BoxSprite().geom(m.props(b.asInstanceOf[Part]))
  m.orientation.getPortSlot(z - c,nports,dims)


def getDiagPortType(): Action[DWD,Option[PortType]] = for
  z <- mousePos
  m <- ops.ask.map(_.$model.now())
  dims <- ops.ask.map(_.dims())
yield
  val o = m.orientation
  val c: Complex = dims/2
  val pt = o.getPortType(z - c)
  val brdr = border(o,pt)
  
  val dl = brdr.dir.dot(z - c) / brdr.dir.dot(dims)
  dl match
    case dl if dl > .33 => Some(OutPort)
    case dl if dl < -.33 => Some(InPort)
    case _ => None

  
  


def initialize(d:DWD): Action[DWD,Unit] = for
  _ <- ops.ask.flatMap(_ => updateModel(_ => d))
  // ns <- ops.ask.map(_.$model.now().parts(Box).toSeq)
  // zs <- randPts(ns.length)
  // _ <- ops.ask.flatMap(
  //   _ => setSubparts(Center,ns.zip(zs))
  // )
yield ()
  


def dragString(b:Entity = Background): Action[DWD,Part] = for
  $m <- ops.ask.map(_.$model)
  z <- mousePos
  pt <- b.entityType match
    case BackgroundType => fromMaybe(getDiagPortType())
    case Box => getBoxPortType(b.asInstanceOf[Part])
    case _ => fromMaybe(ops.pure(None))
  p <- b.entityType match
    case BackgroundType => addPort(Background,pt)
    case Box => addPort(b,pt)
    case _ => fromMaybe(ops.pure(None))
  w <- addWire(p,PropMap().set(Start,z).set(End,z+1))  
  // pi <- b.entityType match
  //   case Box => getBoxPortSlot(b,pt)
  //   case _ => ops.pure($m.now().diagPorts(pt).size)
  drag <- ops.ask.map(_.drag)
  _ <- (for 
    _ <- drag.drag(
      Observer(z => $m.update(
        _.setSubpart(End,w,z)
      ))
    )
    b2Opt <- hoveredPart(Box)
    pt2 <- b2Opt match
      case Some(b2) => getBoxPortType(b2)
      case None => fromMaybe(getDiagPortType())
    // pi2 <- b2Opt match
    //   case Some(b2) => getBoxPortSlot(b2,pt2)
    //   case None => ops.pure($m.now().diagPorts(pt).size)
    p2 <- b2Opt match 
      // case Some(b2) => addBoxPort(b2,w,pt2,pi2)
      case Some(b2) => addPort(b2,pt2)
      case None => addPort(Background,pt2)
    _ <- plug(w,p2)
    _ <- updateModelS(
      aops.remSubpart(End,w)
    )
    _ <- updateModelS(
      aops.remSubpart(Start,w)
    )
  yield ()).onCancelOrError(for
    _ <- ops.delay(drag.$state.set(None))
    _ <- updateModelS(aops.remPart(w))
  yield ())
  _ <- update
yield w






def dragPort(p:Part): Action[DWD,Part] = for
  $m <- ops.ask.map(_.$model)
  // z <- mousePos
  // w <- addWire(PropMap().set(Start,z).set(End,z+1))  
  // pt <- b.entityType match
  //   case BackgroundType => fromMaybe(getDiagPortType())
  //   case Box => getBoxPortType(b.asInstanceOf[Part])
  //   case _ => fromMaybe(ops.pure(None))
  // p <- b.entityType match
  //   case BackgroundType => addDiagPort(w,pt)
  //   case Box => addBoxPort(b,w,pt)
  //   case _ => fromMaybe(ops.pure(None))
  // drag <- ops.ask.map(_.drag)
  // _ <- (for 
  //   _ <- drag.drag(
  //     Observer(z => $m.update(
  //       _.setSubpart(End,w,z)
  //     ))
  //   )
  //   b2Opt <- hoveredPart(Box)
  //   pt2 <- b2Opt match
  //     case Some(b2) => getBoxPortType(b2)
  //     case None => fromMaybe(getDiagPortType())
  //   _ <- log(s"dragString: $pt")
  //   p2 <- b2Opt match 
  //     case Some(b2) => addBoxPort(b2,w,pt2)
  //     case None => addDiagPort(w,pt2)
  //   _ <- updateModelS(
  //     aops.remSubpart(End,w)
  //   )
  //   _ <- updateModelS(
  //     aops.remSubpart(Start,w)
  //   )
  // yield ()).onCancelOrError(for
  //   _ <- ops.delay(drag.$state.set(None))
  //   _ <- updateModelS(aops.remPart(w))
  // yield ())
  // _ <- update
yield p


def setString(strAttr: PValue[String],n: Entity): Action[DWD,Unit] = editStringProp(strAttr)(n.asInstanceOf[Part])



def test(b:Entity): Action[DWD,Unit] = for
  pt <- getDiagPortType()
yield ()


def logModel: Action[DWD,Unit] = for
  m <- ops.ask.map(_.$model.now())
  _ <- log(m.parts)
  _ <- log(m.props)
yield ()


// Bindings



val bindings = Bindings[DWD, Unit](
  // =========== Boxes ===============
  // Add box
  clickOn(ClickType.Double,MouseButton.Left,BackgroundType)
    .flatMap(_ => addBox())
    .map(_ => ()),
  // Drag box
  clickOn(ClickType.Single,MouseButton.Left,Box)
    .flatMap(b => dragPart(b.asInstanceOf[Part])),
  // testing
  clickOn(ClickType.Double,MouseButton.Left,Box)
    .flatMap(b => test(b)),
  // Set box label
  clickOn(ClickType.Double,MouseButton.Left,Box)
    .flatMap(setString(boxLabel,_)),
  // Remove part
  keyDown("Delete").andThen(hovered)
    .flatMap(e => e match
      case Some(p @ Part(_,Port)) => remPort(p)
      case Some(w @ Part(_,Wire)) => remWire(w)
      case Some(b @ Part(_,Box)) => remBox(b)
      case _ => remPart
    ),
  // Drag wire
  clickOn(ClickType.Single,MouseButton.Left,Box)
    .withMods(KeyModifier.Shift)
    .flatMap(box1 => dragString(box1))
    .map(_ => ()),
  clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
    .withMods(KeyModifier.Shift)
    .flatMap(_ => dragString())
    .map(_ => ()),
  // Pan diagram (breaks )
  // clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
  //   .flatMap(_ => dragPan),
  clickOn(ClickType.Single,MouseButton.Right,BackgroundType)
    .flatMap(_ => logModel),
  keyDown("+").andThen(zoomBy(1.1)),
  keyDown("_").andThen(zoomBy(1/1.1)),
)



// =========== Rendering =======================

// Entity & Prop extraction

def extractBoxes(d: DWD): List[(Part,PropMap)] = d.boxes.map(b => 
  val o = d.orientation
  val ps = d.props(b)
  val label = ps.get(boxLabel).getOrElse(s"${b.ob}${b.id}")
  (b, ps.set(Content,label))
).toList





def extractBoxPorts(d: DWD,sprites: Sprites): List[(Part,PropMap)] = d.boxes.toList.flatMap(b =>
    val (spr,props) = sprites(b)
    val (_,dims) = BoxSprite().geom(props)
    val c: Complex = props(Center)
    val o = d.orientation

    d.portsOf(b).toList.map(p =>
      val (ptype,slot) = portSlotB(d,p)
      val n_ports = d.portsOf(b,ptype).size
      val brdr = border(o,ptype)
      val ppos = c + portDelta(brdr,dims,slot,n_ports)

      (p,PropMap()
        .set(Center,ppos)
        .set(Content,"")
        .set(Direction,brdr.dir)
      )
    )
)

def extractDiagPorts(d: DWD,sprites: Sprites,dims:Complex): List[(Part,PropMap)] = 
  val c = dims/2
  val o = d.orientation
 
  d.portsOf(Background).toList.map(p =>
  
    val (ptype,slot) = portSlot(d,p)
    val n_ports = d.portsOf(Background,ptype).size
    val brdr = border(o,ptype)
    val ppos = c + portDelta(brdr,dims,slot,n_ports)

    

    (p,PropMap()
      .set(Center,ppos)
      .set(Content,"")
      // Boundary inputs point in the opposite direction from box inputs
      .set(Direction,-brdr.dir)           
    )
  )


// def extractDiagPorts(d: DWD,sprites: Sprites): List[(Part,PropMap)] = 
//   val (spr,props) = sprites(Background)
//   // val c: Complex = props(Center)
//   val o = d.orientation

//   val boxports = d.portsOf(b).toList.zipWithIndex.map((p,k) =>
//     val (ptype,slot) = portSlot(d,p)
//     val n_ports = d.portsOf(b,ptype).size
//     val brdr = border(o,ptype)
//     val ppos = c + portDelta(brdr,dims,slot,n_ports)

//     (p,PropMap().set(Center,ppos))
// )


def border(o: Orientation,pt: PortType): BoxBorder = (o,pt) match
  case ((LeftToRight,InPort) | (RightToLeft,OutPort)) => Left
  case ((LeftToRight,OutPort) | (RightToLeft,InPort)) => Right
  case ((TopToBottom,InPort) | (BottomToTop,OutPort)) => Top
  case ((TopToBottom,OutPort) | (BottomToTop,InPort)) => Bottom


def portDelta(brdr: BoxBorder, dims: Complex,slot: Int,n_ports: Int) = 
  val frac = -.5 + (.5 + slot)/n_ports
  brdr match
    case Top => Complex(frac * dims.x, -dims.y/2)
    case Bottom => Complex(frac * dims.x, dims.y/2)
    case Left => Complex(-dims.x/2,frac * dims.y)
    case Right => Complex(dims.x/2,frac * dims.y)



def portSlot(d: DWD, e: Entity): (PortType,Int) = e match
  case b @ Part(_,Box) => portSlotB(d,b)
  case _ => portSlotD(d,Background)

def portSlotB(d: DWD, p: Entity): (PortType,Int) = (for
  pt <- d.trySubpart(ptType,p.asInstanceOf[Part])
  b <- d.trySubpart(portBox,p.asInstanceOf[Part])
yield 
  (pt,d.portsOf(b,pt).toSeq.indexOf(p))
) match 
  case Some(pair) => pair
  case None => (OutPort,0)


def portSlotD(d: DWD, p: Entity): (PortType,Int) = d.trySubpart(ptType,p.asInstanceOf[Part])
  .map(pt =>
    val slots = d.portsOf(Background,pt).toSeq
    (pt,slots.indexOf(p))
  ) match 
    case Some(pair) => pair
    case None => (OutPort,0)


def extractWires(d: DWD,sprites:Sprites) = d.wires.toList.map(w =>
  d.portsOf(w).toSeq match
    case Seq(s,t,u @_*) => 
      if u.length > 0 
        then println("Warning: extra wire ports: " + d.portsOf(w))
      (w,wireProps(d,s,t,sprites))
    case Seq(s) => (w,dragProps(d,s,w,sprites))
    case _ => (w,dragProps(d,w,sprites))

)
def dragProps(d: DWD,w:Part,sprites: Sprites) = {
  val zs = d.trySubpart(Start,w).getOrElse(Complex(0,0))
  val zt = d.trySubpart(End,w).getOrElse(Complex(10,10))
  val dir = if zs == zt then one else (zt-zs).normalize
  val dzs = d.trySubpart(StartDir,w).getOrElse(dir)

  PropMap()
    .set(Start,zs)
    .set(StartDir,dzs)
    .set(End,zt - 10*dir)
    .set(Bend,0)
}

def dragProps(d: DWD,s: Part,w:Part,sprites: Sprites) = {
  val zs = sprites(s)._2.get(Center).getOrElse(Complex(0,0))
  val zt = d.trySubpart(End,w).getOrElse(Complex(10,10))
  val dir = if zs == zt then Complex(1,0) else (zt-zs).normalize
  val dzs = sprites(s)._2.get(Direction).getOrElse(dir)

  PropMap()
    .set(Start,zs)
    .set(StartDir,dzs)
    .set(End,zt - 10*dir)
    .set(Bend,0)
}


def wireProps(d: DWD,s: Part, t: Part, sprites: Sprites) = 
  val (sspr,sprops) = sprites(s)
  val (tspr,tprops) = sprites(t)  

  PropMap()
    .set(Start,sprops(Center))
    .set(StartDir,sprops(Direction))
    .set(End,tprops(Center))
    .set(EndDir,tprops(Direction))
    .set(Bend,100)


  // Middleware (controllers & defaults)
def boxMiddleware(
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

def diagPortMiddleware(
  hover:HoverController,
  mouse:MouseController
) = Stack(
  WithDefaults(PropMap() 
    + (Stroke,"none")
    + (Fill, "blue")
    + (MinimumWidth, 15)
    + (MinimumHeight, 15)
    + (InnerSep, 10)
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
) = SpriteMaker[DWD](
  BoxSprite(),
  (d:DWD,spr) => extractBoxes(d),
  boxMiddleware(hover,mouse)
)

def boxPortMaker(
  hover:HoverController,
  mouse:MouseController
) = SpriteMaker[DWD](
  Disc(),
  (d:DWD,spr) => extractBoxPorts(d,spr),
  boxPortMiddleware(hover,mouse)
)

def diagPortMaker(
  hover:HoverController,
  mouse:MouseController,
  dims:Complex
) = SpriteMaker[DWD](
  Disc(),
  (d:DWD,spr) => extractDiagPorts(d,spr,dims),
  diagPortMiddleware(hover,mouse)
)

def wireMaker(hover:HoverController,mouse:MouseController) = SpriteMaker[DWD](
  WireSprite(),
  extractWires,
  wireMiddleware(hover,mouse)
)


// // DOM object (svg)

def renderApp(
  $appModel: Var[DWD],
  hover: HoverController,
  mouse: MouseController,
  dims: Complex
) = {
  val spriteMaps = SpriteMaps[DWD](
    $appModel.signal,                     // : Signal[DWD]
    List(
      boxMaker(hover,mouse),
      boxPortMaker(hover,mouse),
      diagPortMaker(hover,mouse,dims),
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

  // val d: DWD = buildGraph()

  

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      dims <- ops.ask.map(_.dims())
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      // _ <- initialize(buildDiag())
      _ <- addRelative(renderApp($model,hover,mouse,dims))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head")
      .appendChild(styleTag().ref)

    plutoMain[DWD](el,DWD(),serializer,action)
  }
}
























