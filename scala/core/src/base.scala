package semagrams

import com.raquo.laminar.api.L._
import cats.effect._
import cats.effect.std._
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import com.raquo.laminar.codecs.StringAsIsCodec

/** This creates the svg element that will all of the Semagrams activity
  *
  * @todo:
  *   there should be more customization here
  * @todo:
  *   markers should come from elsewhere
  */
def baseSvg() = {
  svg.svg(
    svg.height := "100%",
    svg.width := "100%",
    svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
    svg.style := "border:black;" +
      "border-style:solid;" +
      "background-color:white;" +
      "box-sizing: border-box;",
    svg.defs(
      svg.marker(
        svg.idAttr := "arrowhead",
        svg.markerWidth := "10",
        svg.markerHeight := "7",
        svg.refX := "10",
        svg.refY := "3.5",
        svg.orient := "auto",
        svg.polygon(
          svg.points := "0 0, 10 3.5, 0 7"
        )
      )
    )
  )
}

/** The abstract class that you create an instance of to create a Semagram */
abstract class Semagram {

  /** This is the entrypoint to your app that you should override.
    *
    * @param es
    *   An initialized EditorState to work with
    *
    * @param init
    *   An option of a string to deserialize your state from
    */
  def run(es: EditorState, init: Option[String]): IO[Unit]

  /** Constructs an SVG in `div` and runs your Semagram in there.
    *
    * @param div
    *   The parent div of the root SVG for Semagrams
    *
    * @param init
    *   An optional string representing serialized data to initialize your app
    *   with
    */
  @JSExport
  def main(div: dom.Element, init: js.UndefOr[String]) = {
    val base = baseSvg()
    render(div, base)
    val startup = for {
      eventQueue <- Queue.unbounded[IO, Event]
      es <- Dispatcher.sequential[IO] use { dispatcher =>
        {
          val es = EditorState(base, dispatcher, eventQueue)
          run(es, init.toOption)
        }
      }
    } yield ()
    startup.unsafeRunAndForget()(unsafe.IORuntime.global)
  }
}

/**
  * Towards an embeddable Semagrams:
  *
  * Parts of a Semagram:
  * - Event queue (cats-effect queue, not Laminar)
  * - EditorState
  * - Model (an UndoableVar)
  * - The actual SVG
  *
  * We should have a function which takes in a model, and returns:
  * - EditorState
  * - Event queue
  * - SVG element
  * - A fiber containing the event handling
  *
  * We might want something global playing the role of EditorState, with:
  * - Drag handling (because drags might go across different elements!)
  * - Keyboard events (so that losing focus isn't a problem, though then we have
  *   to figure out what element to send keyboard events to... We could possibly
  *   have something which manually keeps track of what Queue to route keyboard
  *   events to)
  */

// trait Semagram {
//   type Model

//   def apply(m: Var[Model]): IO[(EditorState, Queue[IO, Event], SvgElement)]
// }
