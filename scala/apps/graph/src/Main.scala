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

object GraphDisplay extends Semagram {
  type Model = ACSet

  def layout(g: ACSet) = assignBends(Map(E -> (Src, Tgt)), 0.5)(g)

  val entitySources = Seq(
    ACSetEntitySource(V, Disc()),
    ACSetEdgeSource(E, Src, Tgt, Arrow()),
  )
}

val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left, Set(KeyModifier.Shift)), AddEdgeViaDrag(E, Src, Tgt)),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
)

object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {
      val graphVar = UndoableVar(Graph())
      val stateVar = Var(EditorState(None, Complex(0,0)))
      val globalStateVar = Var(GlobalState(Set()))
      val eventBus = EventBus[Event]()
      val globalEventBus = EventBus[Event]()
      val graphSig = EditorState.modifyACSet(graphVar.signal, stateVar.signal)
      val outbox = EventBus[Message]()

      def display() = GraphDisplay(graphSig, eventBus.writer).amend(
        svg.height := "400px",
        svg.width := "100%",
        svg.style := "border: black; border-style: solid; background-color: white; box-sizing: border-box",
      )

      val mainDiv = div(
        display(),
        globalEventBus.events --> globalStateVar.updater[Event]((globalState, evt) => globalState.processEvent(evt)),
        globalEventBus.events --> eventBus.writer
      )

      GlobalState.listen(globalEventBus.writer)

      render(mountInto, mainDiv)

      val main = for {
        eventQueue <- Queue.unbounded[IO, Event]
        _ <- Dispatcher.sequential[IO] use { dispatcher =>
          mainDiv.amend(
            eventBus.events --> Observer[Event](evt =>
              dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
            )
          )
          Binding.processAll(Action.Resources(graphVar, stateVar, globalStateVar.signal, eventQueue, outbox.writer), bindings)
        }
      } yield ()

      main.unsafeRunAndForget()(unsafe.IORuntime.global)
    }
  }
}
