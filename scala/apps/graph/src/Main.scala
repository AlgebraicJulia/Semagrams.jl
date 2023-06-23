package semagrams.graph

import semagrams.api._
import semagrams.acsets._
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
import semagrams.AddPartAction

// case class LabelFor(e: Entity) extends Entity

// def bindings(es: EditorState, g: Var[Graph], ui: UIState) = {
//   val a = Actions(es, g, ui)

//   Seq(
//     keyDown("a").andThen(a.add_(V, PropMap().set(MinimumWidth, 60).set(MinimumHeight, 60))),
//     keyDown("d").andThen(a.del),
//     keyDown("e").andThen(a.importExport),
//     keyDown("l").andThen(
//       for {
//         mv <- es.hoveredPart(V)
//         _ <- mv.map(v => a.edit(Label, false)(v)).getOrElse(IO(()))
//       } yield ()),
//     clickOnPart(MouseButton.Left, V).withMods().flatMap(a.dragMove),
//     clickOnPart(MouseButton.Left, V)
//       .withMods(KeyModifier.Shift)
//       .flatMap(a.dragEdge(E, Src, Tgt)),
//     dblClickOnPart(MouseButton.Left, V).withMods().flatMap(a.edit(ImageURL, false)),
//   )
// }

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

object Main {
  // TODO:
  //
  // - Bindings
  // - Mouse position
  // - Hovering

  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(div: dom.Element, init: js.UndefOr[String]) = {
      val graphVar = Var(Graph())
      val stateVar = Var(EditorState(None, Complex(0,0)))
      val eventBus = EventBus[Event]()
      val graphSig = graphVar.signal.combineWith(stateVar.signal).map(
        (graph, state) => {
          state.hovered match {
            case Some(ent: Part) => graph.setSubpart(ent, Hovered, ())
            case _ => graph
          }
        }
      )

      val display = GraphDisplay(graphSig, eventBus.writer).amend(
        svg.height := "100%",
        svg.width := "100%",
        svg.style := "border: black; border-style: solid; background-color: white; box-sizing: border-box"
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
          mainLoop(eventQueue, graphVar, stateVar)
        }
      } yield ()

      graphVar.update(_.addPart(V, PropMap() + (Center -> Complex(100, 100)))._1)

      main.unsafeRunAndForget()(unsafe.IORuntime.global)
    }

    val bindings = Seq[Binding[Unit]](
      new Binding[Unit] {
        type EventData = Unit
        val hook = KeyboardHook("a")
        val action = AddPartAction(V)
      }
    )

    def mainLoop(eq: Queue[IO, Event], graphVar: Var[ACSet], stateVar: Var[EditorState]): IO[Unit] =
      Monad[IO].whileM_(IO(true)) {
        for {
          evt <- eq.take
          _ <- IO(stateVar.update(_.processEvent(evt)))
        } yield ()
      }
  }

  // @JSExportTopLevel("GraphApp")
  // object GraphApp extends Semagram {

  //   def run(es: EditorState, init: Option[String]): IO[Unit] = {
  //     val initg = init match {
  //       case Some(s) => read[Graph](s)(ACSet.rw)
  //       case None => Graph()
  //     }
  //     val LabelBox = Box(
  //       PropMap()
  //         + (MinimumHeight, 8)
  //         + (MinimumWidth, 8)
  //         + (InnerSep, 0)
  //         + (OuterSep, 5)
  //     )
  //     for {
  //       g <- IO(Var(initg))
  //       lg <- IO(
  //         g.signal.map(assignBends[SchGraph.type](Map(E -> (Src, Tgt)), 0.35))
  //       )
  //       _ <- es.makeViewport(
  //         lg,
  //         Seq(
  //           ACSetEntitySource(V, BasicDisc(es)),
  //           ACSetEntitySource[SchGraph.type](V, LabelBox).updateEntities(
  //             (e,p) => (
  //               LabelFor(e),
  //               PropMap()
  //                 + (Center, p(Center) + Complex(0,-70))
  //                 + (Content, p.get(Label).getOrElse(""))
  //                 + (Stroke, "none")
  //             )
  //           ),
  //           ACSetEdgeSource(E, Src, Tgt, BasicArrow(es))
  //         )
  //       )
  //       ui <- es.makeUI()
  //       _ <- es.bindForever(bindings(es, g, ui))
  //     } yield ()
  //   }
  // }
}
