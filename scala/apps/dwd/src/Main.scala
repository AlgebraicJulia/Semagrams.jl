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



def bindings(es: EditorState, g: UndoableVar[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui,jsonFromDWD,dwdFromJson)

  val boxMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(Content,false)(b))),
    ("Add Input", (b:Part) => IO(println("addIn"))),
  )

  val wireMenu = Seq(
    ("Rename", (b:Part) => IO(println("rename fired")).>>(a.edit(WireProp.WireLabel,false)(b))),
  )


  def getPort(ent:Entity): IO[Part] = 
    println(s"getPort: $ent")
    ent match
      case Background() => a.add(ROOT,InPort,PropMap())
      case p:Part => p.ty.path match
        case prefix :+ InPort => IO(p)
        case prefix :+ OutPort => IO(p)
        case Seq(Box) => for {
          pt <- getPortType(p)
          _ = println(s"getPortType: $pt")
          p <- a.add(p,pt,PropMap())
        } yield p

  def getPortType(b:Part) = for {
    z <- es.mousePos
    c = g.now().subpart(Center,b)
    pt = if (z-c).x < 0
      then InPort
      else OutPort
  } yield pt


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
      pt <- es.hovered.map(_ match
        case Some(p:Part) => p
        case _ => ROOT
      )
      _ <- pt.ty match
        case ROOT.ty =>
          a.add_(ROOT,OutPort,PropMap())
        case PartType(Seq(Box,_*)) =>
          a.add_(Part(pt.path.slice(0,1)), OutPort, PropMap())
        case _ => 
          IO(println(s"bad add output: $pt"))          
    } yield ()),
    keyDown("i").andThen(for {
      pt <- es.hovered.map(_ match
        case Some(p:Part) => p
        case _ => ROOT
      )
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
    clickOnPart(MouseButton.Left, PartType(Seq(Box)))
      .withMods().flatMap(a.drag),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, OutPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt, Some(getPort))),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt, Some(getPort))),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt, Some(getPort))),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt, Some(getPort))),
    menuOnPart(PartType(Seq(Box)))
      .flatMap(es.makeMenu(ui,boxMenu)),
    menuOnPart(PartType(Seq(Wire)))
      .flatMap(es.makeMenu(ui,wireMenu)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box)))
      .withMods(KeyModifier.Shift).flatMap(b => for {
        p <- a.add(b,OutPort,PropMap())
        _ <- a.dragEdge(Wire,Src,Tgt,Some(getPort))(p)
      } yield ())
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
