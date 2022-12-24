package semagrams.graph

import semagrams.api._
import semagrams.acsets.{_, given}

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

def bindings(es: EditorState, g: Var[Graph], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("a").andThen(a.add(V, PropMap().set(MinimumWidth, 60).set(MinimumHeight, 60))),
    keyDown("d").andThen(a.del),
    keyDown("e").andThen(a.importExport),
    clickOnPart(MouseButton.Left, V).withMods().flatMap(a.drag),
    clickOnPart(MouseButton.Left, V)
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(E, Src, Tgt)),
    dblClickOnPart(MouseButton.Left, V).withMods().flatMap(a.edit(ImageURL, false)),
  )
}

object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp extends Semagram {

    def run(es: EditorState, init: String): IO[Unit] = {
      for {
        g <- IO(Var(read[Graph](init)(ACSet.rw)))
        lg <- IO(
          g.signal.map(assignBends[SchGraph.type](Map(E -> (Src, Tgt)), 0.35))
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(V, BasicDisc(es)),
            ACSetEdgeSource(E, Src, Tgt, BasicArrow(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
