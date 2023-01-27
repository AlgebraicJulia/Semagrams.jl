package semagrams.simplepetri

import semagrams.api._
import semagrams.acsets.{_, given}
import Petris._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

import ACSet._

def bindings(es: EditorState, g: UndoableVar[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("s").andThen(a.addAtMouse(S)),
    keyDown("t").andThen(a.addAtMouse(T)),
    keyDown("d").andThen(a.del),
    keyDown("z")
      .withMods(KeyModifier.Ctrl)
      .andThen(IO(g.undo())),
    keyDown("Z")
      .withMods(KeyModifier.Ctrl, KeyModifier.Shift)
      .andThen(IO(g.redo())),
    clickOnPart(MouseButton.Left, PartType(Seq(S)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(I, IS, IT)),
    clickOnPart(MouseButton.Left, PartType(Seq(T)))
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(O, OT, OS)),
    clickOnPart(MouseButton.Left, PartType(Seq(S))).withMods().flatMap(a.drag),
    clickOnPart(MouseButton.Left, PartType(Seq(T))).withMods().flatMap(a.drag),
    keyDown("e").andThen(for {
                           mx <- es.hoveredPart(Seq(PartType(Seq(S)), PartType(Seq(T))))
                           _ <- mx.map(x => a.edit(Content, false)(x)).getOrElse(IO())
                         } yield ()),
    dblClickOnPart(MouseButton.Left, PartType(Seq(S))).flatMap(a.edit(Content, false)),
    dblClickOnPart(MouseButton.Left, PartType(Seq(T))).flatMap(a.edit(Content, false))
  )
}

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = Petri()
      for {
        g <- IO(UndoableVar(initg))
        lg <- IO(
          g.signal.map(assignBends(Map(I -> (IS, IT), O -> (OT, OS)), 0.5))
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(S, BasicDisc(es)),
            ACSetEntitySource(T, BasicRect(es)),
            ACSetEdgeSource(I, IS, IT, BasicArrow(es)),
            ACSetEdgeSource(O, OT, OS, BasicArrow(es)),
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
