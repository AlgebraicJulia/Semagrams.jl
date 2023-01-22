package semagrams.wiringdiagrams

import semagrams.api._
import semagrams.acsets.{_, given}
import WiringDiagrams._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

def bindings(es: EditorState, g: Var[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("a").andThen(a.addAtMouse(Box, PropMap())),
    keyDown("?").andThen(a.debug),
    keyDown("o").andThen(
      for {
        mb <- es.hoveredPart(PartType(Seq(Box)))
        _ <- mb.map(b => a.add(b, OutPort, PropMap())).getOrElse(IO(()))
      } yield ()),
    keyDown("i").andThen(
      for {
        mb <- es.hoveredPart(PartType(Seq(Box)))
        _ <- mb.map(b => a.add(b, InPort, PropMap())).getOrElse(IO(()))
      } yield ()),
    keyDown("O")
      .withMods(KeyModifier.Shift)
      .andThen(a.add(ROOT, OutPort, PropMap())),
    keyDown("I")
      .withMods(KeyModifier.Shift)
      .andThen(a.add(ROOT, InPort, PropMap())),
    keyDown("d").andThen(a.del),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, OutPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, OutPort)))
      .withMods(KeyModifier.Ctrl)
      .flatMap(a.dragEdge(OutWire, OutSrc, OutTgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(InWire, InSrc, InTgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box))).withMods().flatMap(a.drag),
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

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = WiringDiagram()
      for {
        g <- IO(Var(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal).map(layoutPorts)
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, BasicDPBox(InPort, OutPort)(es, g)),
            ACSetEntitySource(InPort, BasicWireStub(15)(es, g)),
            ACSetEntitySource(OutPort, BasicWireStub(-15)(es, g)),
            ACSetEdgeSource(Wire, Src, Tgt, BasicWire(es, g)),
            ACSetEdgeSource(InWire, InSrc, InTgt, BasicWire(es, g)),
            ACSetEdgeSource(OutWire, OutSrc, OutTgt, BasicWire(es, g)),
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
