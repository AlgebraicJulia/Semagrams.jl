package semagrams.controllers

import semagrams._
import semagrams.util._
import cats.data._
import cats.effect._

import com.raquo.laminar.api.L._

/** A wrapper around a `Var` containing state that keeps track of the current
  * object being dragged. This has to be global because it is easy for something
  * that is being dragged to not be under the mouse, so the mouse-move events
  * have to come from the global window.
  *
  * When the state is non-None, mouse move events get their position sent to the
  * observer. On mouse-up or when the mouse leaves the parent svg, the drag-end
  * handler is called and the state is reset to be None.
  */
class DragController() extends Controller {
  import DragController.State

  val state = Var[Option[State]](None)

  /** Processes mouse events by updating state.
    *
    * @param state
    *   the current state
    * @param evt
    *   the event to process
    */
  def processEvent(
      state: Option[State],
      evt: Event
  ): Option[State] = {
    state.flatMap(h =>
      evt match {
        case MouseMove(pos) => {
          h.observer.onNext(pos)
          Some(h)
        }
        case MouseUp(_, _) => {
          h.callback(())
          None
        }
        case MouseLeave(_) => {
          h.callback(())
          None
        }
        case _ => {
          Some(h)
        }
      }
    )
  }

  /** Returns an IO action that completes when the drag is over
    *
    * @param observer
    *   the observer that gets updated mouse positions during the drag
    */
  def drag[Model](observer: Observer[Complex]): IO[Unit] =
    IO.async_(cb => {
      state.set(Some(State(observer, _ => cb(Right(())))))
    })

  /** Returns an IO action that starts a drag, and completes immediately
    *
    * @param observer
    *   the observer that gets updated mouse positions during the drag
    */
  def dragStart[Model](observer: Observer[Complex]): IO[Unit] =
    IO.delay(state.set(Some(State(observer, _ => ()))))

  /** Hooks up the event stream from editor state to update the drags
    *
    * @param es
    *   the EditorState that the events come from
    * @param elt
    *   the element that the binding is attached to
    */
  def apply(es: EditorState, elt: SvgElement) = elt.amend(
    es.events --> state.updater(processEvent)
  )
}

object DragController {

  /** The state of the DragController, typically contained in a Var
    *
    * @param observer
    *   a function that is called with the new mouse position when the mouse
    *   moves
    * @param callback
    *   a function that is called once when the drag finishes
    */
  case class State(
      observer: Observer[Complex],
      callback: Unit => Unit
  )

  /** Creates a new DragController
    */
  def apply() = {
    new DragController()
  }
}
