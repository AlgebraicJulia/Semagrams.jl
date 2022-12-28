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
import semagrams.widgets._
import Complex.{one,im}
import scala.math.max

import upickle.default._

// ================== Dataypes & Enums ======================


import PortType._
import Orientation._
import BoxBorder._
import org.scalajs.dom.SVGRectElement



enum PortType(porttype:String):
  case InPort extends PortType("in")
  case OutPort extends PortType("out")

  def rev = this match
    case InPort => OutPort
    case OutPort => InPort

object PortType {
  implicit val rw: ReadWriter[PortType] = macroRW
}

case class Slot(ptype: PortType, index: Int) {
  def incr(): Slot = Slot(ptype,index+1)
  def decr(): Slot = Slot(ptype,max(0,index-1))
  def rev() = Slot(ptype.rev,index)

}

object Slot {
  implicit val rw: ReadWriter[Slot] = macroRW
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
  def getBoxPortType(z: Complex) = if z.dot(dir) >= 0
    then OutPort
    else InPort
  
  def getDiagPortType(z: Complex,dims: Complex): Option[PortType] = if z.dot(dir).abs > .35 * dims.dot(dir).abs 
    then Some(getBoxPortType(z))
    else None

  def getSlot(z:Complex,ptype:PortType,nports:Int,dims:Complex): Slot =

    val port_dir = -im * dir
    val z_shift = z.dot(port_dir)
    val start = -dims.dot(port_dir)/2
    val incr = -start/nports

    val slot = this match
      case TopToBottom | RightToLeft => (for
        i <- 0 to nports-1
        spot = start + (1 + 2*i)*incr
        if z_shift < spot
      yield i)
      case BottomToTop | LeftToRight => (for
        i <- (0 to nports-1).reverse
        spot = start + (1 + 2*i)*incr
        if z_shift > spot
      yield i).reverse

    slot.toSeq match
      case Seq() => Slot(ptype,nports)
      case Seq(idx,_*) => Slot(ptype,idx)
    


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
case object portWire extends HomWithDom {
  val dom = Port
  val codom = Wire
}
case object portSlot extends AttrWithDom with PValue[Slot] {
  val dom = Port
}

case object Diag extends Ob
case object diagOrientation extends AttrWithDom with PValue[Orientation] {
  val dom = Diag
}



// Externals (schema object & type)

case object StringSchemaOb extends StaticSchema {
  val schema = BasicSchema(
    Box,boxLabel,Wire,wireLabel,
    Port,portBox,portWire,portSlot,
    Diag,diagOrientation
  )
}
type StringSchema = StringSchemaOb.type


// ========= Graphs (type alias + extensions) =============





type DWD = ACSet[StringSchema]
  extension (d:ACSet[StringSchema]) {
    def boxes: Seq[Part] = d.parts(Box).toSeq
    def wires: Seq[Part] = d.parts(Wire).toSeq
    def ports: Seq[Part] = d.parts(Port).toSeq
    def boxOf(p: Part): Option[Part] = d.trySubpart(portBox,p)
    def wireOf(p: Part): Option[Part] = d.trySubpart(portWire,p)
    def slotOf(p: Part): Option[Slot] = d.trySubpart(portSlot,p)
    
    def labelOf(p: Part): Option[String] = p.entityType match
      case Box => d.props(p).get(boxLabel)
      case Wire => d.props(p).get(wireLabel)
      case _ => None
    

    def portsOf(pt: Part): Seq[Part] = pt match
      case w @ Part(_,Wire) => ports.filter(p => wireOf(p) == Some(w))
      case b @ Part(_,Box) => ports.filter(p => boxOf(p) == Some(b))
      case bg @ Part(_,Diag) => ports.filter(p => boxOf(p) == None)
      case _ => Seq()
    
    def portsOf(pt: Part,ptype:PortType): Seq[Part] = portsOf(pt).map(p => (p,slotOf(p)))
      .collect({ 
        case (p,Some(slot)) if slot.ptype == ptype => p })
    
    def endPt(p: Part): Option[Part] = wireOf(p).flatMap(w => 
      portsOf(w).filterNot(_==p).headOption
    )

    def background: Part = d.parts(Diag).toSeq match
      case Seq() => Part(0,Diag)
      case Seq(bg, _*) => bg
    
         
    def orientation: Orientation = d.trySubpart(diagOrientation,background) match
      case Some(o) => o
      case None => TopToBottom
    
    
        
}

object DWD {
  def apply(orientation: Orientation=Orientation.LeftToRight) = 
    val (opt,_) = ACSet[StringSchema]().addPart(Diag,PropMap().set(diagOrientation,orientation))
    opt
}

//================== Interaction ========================

// Actions

val ops = Action.ops[DWD]
val aops = summon[ACSetOps[StringSchema]]




// Bindings



val bindings = Bindings[DWD, Unit](
  // testing
  keyDown("t").andThen(hovered)
    .flatMap(e => test(e)),
  clickOn(ClickType.Single,MouseButton.Right,BackgroundType)
    .flatMap(_ => logModel),
  // Construction
  clickOn(ClickType.Double,MouseButton.Left,BackgroundType)
    .flatMap(_ => addBoxPos())
    .map(_ => ()),
  // // Remove part
  keyDown("Delete").andThen(hovered)
    .flatMap(e => e match
      case Some(p @ Part(_,Port)) => remPort(p)
      case Some(w @ Part(_,Wire)) => remWire(w)
      case Some(b @ Part(_,Box)) => remBox(b)
      case _ => ops.pure(())
    ),
  // Drag wire
  clickOnPart(ClickType.Single,MouseButton.Left)
    .withMods(KeyModifier.Shift)
    .flatMap(part => dragStringFrom(part))
    .map(_ => ()),
  clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
    .withMods(KeyModifier.Shift)
    .flatMap(_ => dragStringFromDiag())
    .map(_ => ()),
  // Drag box
  clickOn(ClickType.Single,MouseButton.Left,Box)
    .flatMap(b => dragPart(b.asInstanceOf[Part])),
  // Set box label
  clickOn(ClickType.Double,MouseButton.Left,Box)
    .flatMap(b => setString(boxLabel,b.asInstanceOf[Part])),
  // // Pan & Zoom (breaks port placement)
  // clickOn(ClickType.Single,MouseButton.Left,BackgroundType)
  //   .flatMap(_ => dragPan),
  // keyDown("+").andThen(zoomBy(1.1)),
  // keyDown("_").andThen(zoomBy(1/1.1)),
)




// Actions

// Testing


def init(): Action[DWD,Unit] = for
  dims <- ops.ask.map(_.dims())
  
  bg <- background()
  p00 <- addPort(bg,Slot(InPort,0))
  p01 <- addPort(bg,Slot(InPort,1))

  b1 <- addBox(dims/3,PropMap().set(boxLabel,"F"))
  p10 <- addPort(b1,Slot(InPort,0))
  p11 <- addPort(b1,Slot(OutPort,0))
  p12 <- addPort(b1,Slot(OutPort,1))

  b2 <- addBox(2*dims/3,PropMap().set(boxLabel,"G"))
  p20 <- addPort(b2,Slot(InPort,0))
  p21 <- addPort(b2,Slot(InPort,1))
  p22 <- addPort(b2,Slot(OutPort,0))

  p30 <- addPort(bg,Slot(OutPort,0))
  p31 <- addPort(bg,Slot(OutPort,1))
  
  _ <- addWire(p12,p20,PropMap().set(wireLabel,"X"))
  _ <- addWire(p00,p10)
  _ <- addWire(p11,p30)
  _ <- addWire(p01,p21)
  _ <- addWire(p22,p31)
  

yield ()


def test(eOpt:Option[Entity]): Action[DWD,Unit] = eOpt match
  case None => (for
    _ <- log("test bg")
    slot <- getDiagSlotPos()
    _ <- log(slot)
  yield ())
  case Some(e) => e.entityType match
    case Box => (for 
      _ <- log(s"test box: $e")
      m <- modelNow()
      _ <- log(m.portsOf(e.asInstanceOf[Part],InPort).map(p => (p,m.slotOf(p))))
      _ <- log(m.portsOf(e.asInstanceOf[Part],OutPort).map(p => (p,m.slotOf(p))))
    yield ())
    case Wire => (for 
      _ <- log(s"test wire: $e")
    yield ())
    case Port => (for
      _ <- log(s"test port: $e")
    yield ())
    case x => log(s"?? unknown test $x")


// Creation

def addBox(z:Complex,props: PropMap = PropMap()): Action[DWD,Part] = ops.ask.flatMap(
  _ => addPart(Box,props.set(Center,z))
)

def addBoxPos(props:PropMap = PropMap()) = mousePos.flatMap(z => addBox(z,props))

def addWire(src:Part,tgt:Part,props:PropMap = PropMap()): Action[DWD,Part] = (src.entityType,tgt.entityType) match
  case (Port,Port) => (for
    w <- ops.ask.flatMap(_ => addPart(Wire,props))
    _ <- ops.ask.flatMap(_ => setSubpart(portWire,src,w))
    _ <- ops.ask.flatMap(_ => setSubpart(portWire,tgt,w))
    _ <- delay(.1)
  yield w)

def addPort(part:Part,slot:Slot,props:PropMap = PropMap()): Action[DWD,Part] = for
  _ <- part.entityType match
    case Box | Diag => ops.ask.flatMap(_ => addSlot(part,slot))
  p <- part.entityType match
    case Box => ops.ask.flatMap(_ => addPart(Port,props.set(portSlot,slot).set(portBox,part)))
    case Diag => ops.ask.flatMap(_ => addPart(Port,props.set(portSlot,slot)))
yield p


def addPortPos(part:Part,props:PropMap = PropMap()): Action[DWD,Part] = for
  slot <- part.entityType match
    case Box => ops.ask.flatMap(_ => getBoxSlotPos(part))
    case Diag => ops.ask.flatMap(_ => getDiagSlotPos())
  p <- addPort(part,slot)
yield p


def addPortZ(z:Complex,props:PropMap = PropMap()): Action[DWD,Part] = ops.ask.flatMap(
  _ => addPart(Port,props.set(Center,z))
)




// Port insertion

def insertPort(port:Part, part:Part,slot:Slot): Action[DWD,Unit] = for
  _ <- part.entityType match
    case Box | Diag => ops.ask.flatMap(_ => addSlot(part:Part,slot:Slot))
  _ <- part.entityType match
    case Box => (for 
      _ <- ops.ask.flatMap(_ => setSubpart(portSlot,port,slot))
      _ <- ops.ask.flatMap(_ => setSubpart(portBox,port,part))
    yield ())
    case Diag => ops.ask.flatMap(_ => setSubpart(portSlot,port,slot))
yield ()

def insertPortPos(port:Part,part:Part): Action[DWD,Unit] = for
  slot <- part.entityType match
    case Box => getBoxSlotPos(part)
    case Diag => getDiagSlotPos()
  _ <- insertPort(port,part,slot)  
yield ()

def addSlot(part:Part,slot:Slot): Action[DWD,Unit] = for
  m <- modelNow()
  pslots = m.portsOf(part,slot.ptype)
    .map(p => (p,m.slotOf(p)))
    .collect({ case (p,Some(pslot)) if pslot.index >= slot.index => (p,pslot) })      
  _ <- ops.ask.flatMap(_ => doAll(pslots.map({ case (p,pslot) => setSubpart(portSlot,p,pslot.incr()) })))
yield ()


def mergePorts(anchored:Part,dragged:Part) = for
  m <- modelNow()
  anchor = m.boxOf(anchored) match
    case Some(b) => b
    case None => m.background
  p_fin <- m.wireOf(anchored) match
    // Already wired
    case Some(w) => insertPortPos(dragged,anchor)
    // Safe to merge
    case None => m.slotOf(anchored) match
      case Some(slot) => (for
        // Don't use remWire b/c it rearranges ports
        _ <- ops.ask.flatMap(_ => updateModelS(aops.remPart(anchored)))
        _ <- ops.ask.flatMap(_ => setSubpart(portSlot,dragged,slot))
        _ <- m.boxOf(anchored) match
          case Some(b) => ops.ask.flatMap(_ => setSubpart(portBox,dragged,b))
          case None => ops.pure(())
      yield dragged)
      case None => insertPortPos(dragged,anchor)
yield dragged


// Port removal

def unplug(plug: Part,socket: Part): Action[DWD,Unit] = for
  m <- modelNow()  
  _ <- (plug.entityType,socket.entityType) match
    case (Port,Wire) => unplugPortWire(plug,socket)
    case (Wire,Port) => unplugPortWire(socket,plug)
    case (Port,Box) => unplugPortBox(plug,socket)
    case (Box,Port) => unplugPortBox(socket,plug)
    case (Port,Diag) => unplugPortDiag(plug)
    case (Diag,Port) => unplugPortDiag(socket)
yield ()

def unplugPortWire(p:Part,w:Part) = for
  m <- modelNow()
  if m.wireOf(p) == Some(w)
  _ <- ops.ask.flatMap(_ => remSubpart(portWire,p))
yield ()

def unplugPortBox(p:Part,b:Part) = for 
  m <- modelNow()
  if m.boxOf(p) == Some(b)
  _ <- ops.ask.flatMap(_ => remSubpart(portBox,p))
  slotOpt = m.slotOf(p)
  _ <- slotOpt match
    case None => ops.pure(())
    case Some(slot) => (for
      _ <- ops.ask.flatMap(_ => remSubpart(portSlot,p))
      _ <- ops.ask.flatMap(_ => removeSlot(b,slot))
    yield ())
yield ()

def unplugPortDiag(p:Part) = for 
  m <- modelNow()
  if m.boxOf(p) == None
  slot = m.slotOf(p)
  _ <- slot match
    case None => ops.pure(())
    case Some(sl) => (for
      _ <- ops.ask.flatMap(_ => remSubpart(portSlot,p))
      _ <- ops.ask.flatMap(_ => removeSlot(m.background,sl))
    yield ())
yield ()

def removeSlot(part:Part,slot:Slot): Action[DWD,Unit] = for
  m <- modelNow()
  pslots = m.portsOf(part,slot.ptype)
    .map(p => (p,m.slotOf(p)))
    .collect({ case (p,Some(pslot)) if pslot.index > slot.index => (p,pslot) })
  _ <- ops.ask.flatMap(_ => doAll(pslots.map({ case (p,pslot) => setSubpart(portSlot,p,pslot.decr())})))
yield ()


// Dragging



def dragString(): Action[DWD,Part] = for
  bg <- background()
  w <- dragStringFrom(bg)
yield w


def dragStringFrom(src:Part): Action[DWD,Part] = src.entityType match
  case Box => dragStringFromBox(src)
  case Diag => dragStringFromDiag()
  case Port => dragStringFromPort(src)

def dragStringFromBox(b:Part): Action[DWD,Part] = for
  p1 <- addPortPos(b)
  p2 <- mousePos.flatMap(z => addPortZ(z))
  w <- addWire(p1,p2)
  _ <- dragPort(p2)
yield w

def dragStringFromDiag(): Action[DWD,Part] = for
  bg <- background()
  p1 <- addPortPos(bg)
  p2 <- mousePos.flatMap(z => addPortZ(z))
  w <- addWire(p1,p2)
  _ <- dragPort(p2)
yield w


def dragStringFromPort(p1:Part): Action[DWD,Part] = for
  m <- modelNow()
  w <- m.wireOf(p1) match
    // Already wired => unplug & drag
    case Some(w) => (for
      _ <- m.boxOf(p1) match
        case Some(b) => unplug(p1,b)
        case None => unplug(p1,m.background)    
      z <- mousePos
      _ <- ops.ask.flatMap(_ => setSubpart(Center,p1,z))
      _ <- dragPort(p1)
    yield w)
    // No wire => add new port & drag
    case None => (for
      p2 <- mousePos.flatMap(z => addPortZ(z))
      w <- addWire(p1,p2)
      _ <- dragPort(p2)
    yield w)
yield w


def dragPort(p2:Part): Action[DWD,Unit] = for
  $m <- ops.ask.map(_.$model)
  z0 <- mousePos
  drag <- ops.ask.map(_.drag)
  _ <- (for 
    _ <- drag.drag(
      Observer(z => $m.update(
        _.setSubpart(Center,p2,z + 10*(z0 - z).normalize)
          .setSubpart(Direction,p2,(z - z0).normalize)
      ))
    )
    target <- hovered
    _ <- target match
      case None => insertPortPos(p2,$m.now().background)
      case Some(b2) => b2.entityType match
        case Box => insertPortPos(p2,b2.asInstanceOf[Part])
        case Port => mergePorts(b2.asInstanceOf[Part],p2)
    _ <- updateModelS(
      aops.remSubpart(Center,p2)
    )
    _ <- updateModelS(
      aops.remSubpart(Direction,p2)
    )
  yield ()).onCancelOrError(for
    _ <- ops.delay(drag.$state.set(None))
  yield ())
  _ <- update
yield ()


// Deleting

def remPort(p: Part): Action[DWD, Unit] = for
  m <- modelNow()
  boxOpt = m.boxOf(p)
  _ <- boxOpt match
    case Some(b) => remBoxPort(p,b)
    case None => remDiagPort(p)
yield ()

def remBoxPort(p:Part,b:Part) = for
  m <- modelNow()
  if m.boxOf(p) == Some(b)
  _ <- unplug(p,b)
  _ <- insertPortPos(p,m.background)
yield ()

def remDiagPort(p:Part) = for
  m <- modelNow()
  wOpt = m.wireOf(p)
  _ <- wOpt match
    case None => ops.pure(())
    case Some(w) => remWire(w)
  _ <- updateModelS(aops.remPart(p))
yield ()

def remWire(w: Part): Action[DWD,Unit] = for
  m <- modelNow()
  ps = m.portsOf(w)
  _ <- ops.ask.flatMap(_ => doAll(ps.map(p =>
    unplug(p,w)  
  )))
  _ <- ops.ask.flatMap(_ =>
    updateModelS(aops.remPart(w))
  )
yield ()

def remBox(b: Part): Action[DWD,Unit] = for
  m <- modelNow()
  bg <- background()
  ps = m.portsOf(b)
  _ <- doAll(ps.map(p => (for
    _ <- ops.ask.flatMap(_ => remSubpart(portBox,p))
    _ <- ops.ask.flatMap(_ => insertPortPos(p,bg))
  yield ())))
  _ <- ops.ask.flatMap(_ =>
    updateModel(_.remPart(b))
  )
yield ()



// Helpers

def modelNow() = ops.ask.map(_.$model.now())
def background() = modelNow().map(_.background)

def setString(strAttr: PValue[String],pt: Part): Action[DWD,Unit] = editStringProp(strAttr)(pt)



def logModel: Action[DWD,Unit] = for
  m <- modelNow()
  _ <- log(m.parts)
  _ <- doAll(m.props.toSeq.map(log))
yield ()



// Hacky. Doesn't check box ID b/c I don't know how to find it
def getBoxDims(b:Part): Action[DWD,Complex] = for
  rects <- ops.ask.map(
    _.playArea.ref.children.flatMap(_.children).flatMap(_.children)
      .filter(_.nodeName=="rect").map(_.asInstanceOf[SVGRectElement])
  )
  dims = rects.map(r => Complex(r.width.baseVal.value,r.height.baseVal.value))
  bs <- modelNow().map(_.boxes)
yield 
  bs.zip(dims).find({case (bi,d) => bi==b}) match
    case Some((_,z)) => z
    case None => 
      Complex(40,40)

def getBoxSlotPos(b:Part): Action[DWD,Slot] = for
  m <- modelNow()
  dims <- getBoxDims(b)
  z <- mousePos
  o = m.orientation
  c = m.props(b)(Center)
  ptype = o.getBoxPortType(z - c)
  n_pts = m.portsOf(b,ptype).size
yield o.getSlot(z-c,ptype,n_pts,dims)

def getDiagSlotPos(): Action[DWD,Slot] = for
  m <- modelNow()
  dims <- ops.ask.map(_.dims())
  z <- mousePos
  o = m.orientation
  c = dims/2
  ptype = o.getBoxPortType(z - c)
  n_pts = m.portsOf(m.background,ptype).size
yield o.getSlot(z-c,ptype,n_pts,dims)


// =========== Rendering =======================

// Part & Prop extraction

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

  d.portsOf(b).toList.map(p => (p,d.slotOf(p))).collect({
    case (p,Some(pslot)) =>
      val Slot(ptype,slot) = pslot
      val n_ports = d.portsOf(b,ptype).size
      val brdr = border(o,ptype)
      val ppos = c + portDelta(brdr,dims,slot,n_ports)

      (p,PropMap()
        .set(Center,ppos)
        .set(Content,s"")
        .set(Direction,brdr.dir)
      )
  })
)


def extractDiagPorts(d: DWD,sprites: Sprites,dims: Complex): List[(Part,PropMap)] = 
  val c: Complex = dims/2
  val o = d.orientation
  val bg = d.background

  d.portsOf(bg).toList.map(p => 
    (d.slotOf(p),d.props(p).get(Center)) match
      case (Some(pslot),None) =>
        val Slot(ptype,slot) = pslot
        val n_ports = d.portsOf(bg,ptype).size
        val brdr = border(o,ptype)
        val ppos = c + portDelta(brdr,dims,slot,n_ports)

        (p,PropMap().set(Center,ppos).set(Content,s"").set(Direction,-brdr.dir))
      case (_,Some(z)) => (p,PropMap().set(Center,z).set(Content,s""))
      case _ => (p,PropMap().set(Center,dims/2).set(Content,"No slot"))
  )
        


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


def extractWires(d: DWD,sprites:Sprites) = d.wires.toList
  .map(w => (w,d.portsOf(w)))
  .map({
    case (w,Seq(s,t)) => (w,wireProps(d,w,s,t,sprites))
    case (w,badports) => 
      println(s"Bad port assignment: $w -> ${d.portsOf(w)}")
      (w,PropMap().set(Start,Complex(0,0)).set(End,Complex(100,100)))
  })


def wireProps(d: DWD,w: Part,s: Part, t: Part, sprites: Sprites): PropMap = {
  val (_,sprops) = sprites(s)
  val (_,tprops) = sprites(t)

  val zs = sprops(Center)
  val zt = tprops(Center)
  val dzs = sprops.get(Direction).getOrElse((zt - zs).normalize)
  val dzt = tprops.get(Direction).getOrElse((zs - zt).normalize)
  val label = d.labelOf(w).getOrElse(s"${w.ob}${w.id}")

  PropMap()
    .set(Start,zs).set(End,zt)
    .set(StartDir,dzs).set(EndDir,dzt)
    .set(WireLabel,label)
}


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
    + (MinimumWidth, 12)
    + (MinimumHeight, 12)
    + (InnerSep, 5)
    + (FontSize, 16)
    + (Style, "filter: opacity(0.5)")
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
    + (MinimumWidth, 18)
    + (MinimumHeight, 18)
    + (InnerSep, 10)
    + (FontSize, 16)
    + (Style, "filter: opacity(0.5)")
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
  WithDefaults(PropMap() + (Stroke,"black") + (Bend,100)
),
  Hoverable(hover,MainHandle,
    PropMap() + (Style, "filter: opacity(1)")
  ),
  Clickable(mouse, MainHandle)
)

// Sprite makers


def boxMaker(hover:HoverController,mouse:MouseController) = 
  SpriteMaker[DWD](BoxSprite(),(d:DWD,spr) => extractBoxes(d),boxMiddleware(hover,mouse))

def boxPortMaker(hover:HoverController,mouse:MouseController) = 
  SpriteMaker[DWD](Disc(),(d:DWD,spr) => extractBoxPorts(d,spr),boxPortMiddleware(hover,mouse))

def diagPortMaker(hover:HoverController,mouse:MouseController,dims:Complex) = 
  SpriteMaker[DWD](Disc(),(d:DWD,spr) => extractDiagPorts(d,spr,dims),diagPortMiddleware(hover,mouse))

def wireMaker(hover:HoverController,mouse:MouseController) = 
  SpriteMaker[DWD](WireSprite(),extractWires,wireMiddleware(hover,mouse))


// // DOM object (svg)

def renderApp($appModel: Var[DWD],hover: HoverController,mouse: MouseController,dims: Complex) = {
  val spriteMaps = SpriteMaps[DWD]($appModel.signal,
    List(boxMaker(hover,mouse),boxPortMaker(hover,mouse),
      diagPortMaker(hover,mouse,dims),wireMaker(hover,mouse)
    )
  )
  svg.g(
    spriteMaps.attach,
    wrappedHtml(
      button("Copy to clipboard",
        // disabled <-- disableBtnVar,
        typ:="button",
        onClick --> (_ => {
          dom.window.navigator.clipboard.writeText(
            $appModel.now().toSerializable.toString()
          )
          println("copied to clipboard")
        })
      ),
      Complex(0,0),
      Complex(100,100)
    )
  )

}




//================ Main =====================

val serializer = ACSet.rw[StringSchema]

object Main {


  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      dims <- ops.ask.map(_.dims())
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- ops.ask.flatMap(_ => init())
      _ <- addRelative(renderApp($model,hover,mouse,dims))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head")
      .appendChild(styleTag().ref)

    plutoMain[DWD](el,DWD(),serializer,action)
  }
}
























