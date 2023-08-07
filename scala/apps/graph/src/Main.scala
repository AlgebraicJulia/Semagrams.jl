package semagrams.graph

import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.listeners._
import Graphs._

import upickle.default._
import com.raquo.laminar.api.L._
import cats._
import cats.syntax._
import cats.effect._
import cats.effect.std._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import com.raquo.laminar.defs.eventProps.EventProps
import com.raquo.laminar.codecs.StringAsIsCodec
import org.scalajs.dom.SVGSVGElement

object GraphDisplay extends Semagram {
  type Model = ACSet

  def layout(g: ACSet) = g

  def produceSprites(g: ACSet, eventWriter: Observer[Event]) = {
    EntityCollector.collect(
      g,
      Seq(
        ACSetEntitySource(V, Disc()),
        ACSetEdgeSource(E, Src, Tgt, Arrow()),
      )
    )
  }
}

val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag())
)

object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(div: dom.Element, init: js.UndefOr[String]) = {
      val graphVar = UndoableVar(Graph())
      val stateVar = Var(EditorState(None, Complex(0,0)))
      val globalStateVar = Var(GlobalState(Set()))
      val eventBus = EventBus[Event]()
      val graphSig = EditorState.modifyACSet(graphVar.signal, stateVar.signal)

      val display = GraphDisplay(graphSig, eventBus.writer).amend(
        svg.height := "100%",
        svg.width := "100%",
        svg.style := "border: black; border-style: solid; background-color: white; box-sizing: border-box",
      )

      GlobalState.listen(eventBus.writer)

      render(div, display)

      val main = for {
        eventQueue <- Queue.unbounded[IO, Event]
        _ <- Dispatcher.sequential[IO] use { dispatcher =>
          display.amend(
            eventBus.events --> Observer[Event](evt =>
              dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
            )
          )
          Binding.processAll(Action.Resources(graphVar, stateVar, globalStateVar, eventQueue), bindings)
        }
      } yield ()

      main.unsafeRunAndForget()(unsafe.IORuntime.global)
    }
  }
}
