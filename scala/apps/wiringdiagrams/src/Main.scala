package semagrams.wiringdiagrams

import semagrams.api._
import semagrams.acsets.{_, given}
import WiringDiagrams._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._

val mkAdd = for {
  _ <- addPart(OutPort)
  _ <- addPart(InPort)
  _ <- addPart(InPort)
  _ <- setProp(ROOT, Content, "+")
} yield ()

val mkZero = for {
  _ <- addPart(OutPort)
  _ <- setProp(ROOT, Content, "0")
} yield ()

val monoidOps = Seq(
  ("Add", mkAdd.run(WiringDiagram()).value._1),
  ("Zero", mkZero.run(WiringDiagram()).value._1)
)

def bindings(es: EditorState, g: UndoableVar[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("a").andThen(for {
      choice <- ui.dialogue[ACSet](cb =>
        PositionWrapper(Position.botMid(10), Select(monoidOps)(cb))
      )
      _ <- a.addAtMouse_(Box, choice)
    } yield ()),
    keyDown("?").andThen(a.debug),
    keyDown("o").andThen(for {
      mb <- es.hoveredPart(PartType(Seq(Box)))
      _ <- mb.map(b => a.add_(b, OutPort, PropMap())).getOrElse(IO(()))
    } yield ()),
    keyDown("i").andThen(for {
      mb <- es.hoveredPart(PartType(Seq(Box)))
      _ <- mb.map(b => a.add_(b, InPort, PropMap())).getOrElse(IO(()))
    } yield ()),
    keyDown("O")
      .withMods(KeyModifier.Shift)
      .andThen(a.add_(ROOT, OutPort, PropMap())),
    keyDown("I")
      .withMods(KeyModifier.Shift)
      .andThen(a.add_(ROOT, InPort, PropMap())),
    keyDown("d").andThen(a.del),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    clickOnPart(MouseButton.Left, PartType(Seq(Box, OutPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(InPort)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box)))
      .withMods()
      .flatMap(a.dragMove),
    dblClickOnPart(MouseButton.Left, PartType(Seq(Box)))
      .flatMap(a.edit(Content, false))
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
      { case ((p, sub), i) =>
        (p, sideCenter + spacer.assignPos(i, n) * im)
      }
    )
    cs.foldLeft(acs)((acs, pc) => acs.setProp(pc._1, Center, pc._2))
  }
  helper(helper(init, InPort, -1), OutPort, 1)
}

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = WiringDiagram()
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          es.size.signal.combineWith(g.signal).map(layoutPorts)
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, BasicDPBox(InPort, OutPort)(es)),
            ACSetEntitySource(InPort, BasicWireStub(15)(es)),
            ACSetEntitySource(OutPort, BasicWireStub(-15)(es)),
            ACSetEdgeSource(Wire, Src, Tgt, BasicWire(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
