package semagrams.graph

import semagrams._
import semagrams.acsets._

import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

val ops = summon[GraphOps[SchGraph.type]]

def addVertexAction(es: EditorState, g: Var[Graph]) = for {
  pos <- es.mousePos
  _ <- g.updateS_(ops.addVertex(PropMap() + (Center, pos)))
} yield ()

def bindings(es: EditorState, g: Var[Graph]) = Seq(
  keyDown("a").andThen(addVertexAction(es, g))
)

object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp extends Semagram {

    /**
     * TODO:
     * - Bindings
     */
    def run(es: EditorState): IO[Unit] = {
      val VertexSprite = WithMiddleware(
        Disc(),
        Seq(
          Hoverable(es.hover, MainHandle, PropMap() + (Fill, "lightgrey")),
        )
      )
      for {
        g <- IO(Var(Graph()))
        mainView <- IO(new Viewport(
            Seq(
              ACSetEntitySource(g.signal, V, VertexSprite)
            )
          ))
        _ <- IO(es.register(mainView))
        _ <- es.bindForever(bindings(es, g))
      } yield ()
    }
  }
}
