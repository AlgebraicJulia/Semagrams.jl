package semagrams.dwd


import semagrams.api._
import semagrams.dblClickOn
import semagrams.acsets.{_, given}

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._
import semagrams.Background




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




def dwdFromJson(s:String): Option[ACSet] = Some(DWD())
def jsonFromDWD(dwd: ACSet): String = ""



def bindings(
  es: EditorState, 
  g: UndoableVar[ACSet], 
  ui: UIState
) = {
  val a = Actions(es, g, ui,jsonFromDWD,dwdFromJson)

  val boxMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,false)(b))),
    ("Add Input", (b:Part) => IO(println("addIn"))),
  )

  val wireMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(WireProp.WireLabel,false)(b))),
  )





  def getPort(ent:Entity): IO[Part] = 
    ent match
      // case Background() => for {
      //   (ptype,pnum) <- getBgPortInfo()
      //   p <- a.add(ROOT,ptype,PropMap().set(Content,pnum))
      // } yield p
      case i:Part => i.ty.path match
        case prefix :+ InPort => IO(i)
        case prefix :+ OutPort => IO(i)
        // case Seq(Box) => for {
        //   ptype <- getBoxPortType(i)
        //   p <- a.add(i,ptype,PropMap())
        // } yield p
        case _ => IO(ROOT)


  def getBgPortType() = for {
    z <- es.mousePos
    c = es.size.now()/2
    ptype = if (z-c).x < 0
      then InPort
      else OutPort
  } yield ptype

  def getBgPortInfo = for
    ptype <- getBgPortType()
    nports = g.now().partsMap
      .get(ptype).map(pts => pts.nextId)
      .getOrElse(0)
    size = es.size.now()
    p <- es.mousePos
  yield (ptype,portNumber(p,size,nports))

  def portNumber(pos:Complex,size:Complex,nports:Int) =
    // println(s"portNumber: $pos, $size, $nports")
    val l = (0 to nports+1)
      .map(_ * size.y/(nports + 1))
    // println(s"l = $l")
    // println(s"l = ${l.map(_ < pos.y)}")
      
    l.filter(_ < pos.y).length


  def getBoxPortType(b:Part) = for
    z <- es.mousePos
    c <- fromMaybe(IO(g.now().subacset(b).props.get(Center)))
    ptype = if (z-c).x < 0
      then InPort
      else OutPort
  yield ptype

  def getBoxSize(b:Part) = for 
    p <- es.mousePos
    box = g.now().subacset(b)
    // size = 
  yield println(size)

  def getBoxPortInfo(b:Part) = for 
    ptype <- getBoxPortType(b)
    gx = g.now()
    ports = gx.subacset(b).partsMap(ptype)
  yield println(ports)


  def portFromPos(p:Part): IO[(PartType,Int)] = p.ty.path match
    case Seq() => (for 
      tp <- getBgPortType()
    yield (ROOT.ty.extend(InPort),0))
    case Seq(Box,_*) => (for
      _ <- IO(())
    yield (p.ty.extend(InPort),0))


  Seq(
    // dblClickOnPart(MouseButton.Left,PartType(Seq(Box)))
    //   .flatMap(a.edit(Content,false)),      
    dblClickOn(MouseButton.Left).flatMap(ent => 
      ent match
        case Background() => a.addAtMouse(Box)
        case p:Part => p.ty.path match
          case Seq(Box,_*) => a.edit(Content,false)(p)
          case _ => IO(println(s"dblClickOn: p=$p"))        
        case _ => IO(println(s"dblClickOn: ent=$ent"))
    ),
    keyDown("?").andThen(a.debug),
    keyDown("o").andThen(for {
      pt <- fromMaybe(es.hoveredPart)
      _ <- pt.ty match
        case ROOT.ty =>
          a.add_(ROOT,OutPort,PropMap())
        case PartType(Seq(Box,_*)) =>
          a.add_(Part(pt.path.slice(0,1)), OutPort, PropMap())
        case _ => 
          IO(println(s"bad add output: $pt"))          
    } yield ()),
    keyDown("i").andThen(for {
      pt <- fromMaybe(es.hoveredPart)
      _ <- pt.ty match
        case ROOT.ty =>
          a.add_(ROOT,InPort,PropMap())
        case PartType(Seq(Box,_*)) =>
          a.add_(Part(pt.path.slice(0,1)), InPort, PropMap())
        case _ => 
          IO(println(s"bad add output: $pt"))          
    } yield ()),
    keyDown("d").andThen(a.del),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    // clickOnPart(MouseButton.Left, PartType(Seq(Box)))
    //   .withMods().flatMap(a.dragMove),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, OutPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    // clickOn(MouseButton.Left)
    //   .withMods(KeyModifier.Shift)
      // .flatMap(src => a.makeWire(Wire,Src,Tgt)),
    menuOnPart(PartType(Seq(Box)))
      .flatMap(es.makeMenu(ui,boxMenu)),
    menuOnPart(PartType(Seq(Wire)))
      .flatMap(es.makeMenu(ui,wireMenu)),
    // clickOnPart(MouseButton.Left, PartType(Seq(Box)))
    //   .withMods(KeyModifier.Shift).flatMap(b => for {
    //     pt <- getBoxPortType(b)
    //     p <- a.add(b,pt,PropMap())
    //     _ <- a.dragEdge(Wire,Src,Tgt)(p)
    //   } yield ()),
    clickOn(MouseButton.Left)
      .withMods(KeyModifier.Shift)
      .flatMap(
        ent => ent match
          case Background() => for {
            pt <- getBgPortType()
            p <- a.add(ROOT,pt,PropMap())
            _ <- a.dragEdge(Wire,Src,Tgt)(p)
          } yield ()
      ),
    // clickOn(MouseButton.Left)
    //   .flatMap(_ => getBgPortInfo)
    //   .flatMap(info => IO(println(info))),
    clickOnPart(MouseButton.Left,PartType(Seq(Box)))
      .flatMap(b => getBoxSize(b))
          
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



import semagrams.sprites.{AltDPBox,BasicPort}

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
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, AltDPBox(InPort, OutPort)(es)).withProps(PropMap() + (FontSize,22)),
            ACSetEntitySource(InPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"green")),
            ACSetEntitySource(OutPort, BasicPort()(es)).withProps(PropMap() + (MinimumWidth,40) + (Fill,"red")),
            ACSetEdgeSource(Wire, Src, Tgt, BasicWire(es)),
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
