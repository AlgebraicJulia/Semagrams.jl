package semagrams.graph

import semagrams.api._
import semagrams.acsets._

import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

def bindings(es: EditorState, g: Var[Graph]) = {
  val a = Actions(es, g)

  Seq(
    keyDown("a").andThen(a.add(V)),
    keyDown("d").andThen(a.del),
    clickOnPart(MouseButton.Left, V).withMods().flatMap(a.drag),
    clickOnPart(MouseButton.Left, V)
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(E, Src, Tgt))
  )
}

object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp extends Semagram {

    def run(es: EditorState): IO[Unit] = {
      for {
        g <- IO(Var(Graph()))
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
        _ <- es.bindForever(bindings(es, g))
      } yield ()
    }
  }
}
