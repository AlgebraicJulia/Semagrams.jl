package semagrams.controllers

import semagrams._
import semagrams.util._
import semagrams.actions._
import cats.data._
import cats.effect._

import com.raquo.laminar.api.L._
import monocle.std.these
import com.raquo.airstream.state.ObservedSignal

/** This is meant to be a bit of global state that keeps track of the current
  * object being dragged. The drag handler has to go on the main window, because
  * it is easy for something that is being dragged to not be under the mouse.
  *
  * The state of the drag controller is an option of observer of positions. When
  * this state is non-None, mouse move events get their position sent to the
  * observer. On mouse-up or when the mouse leaves the parent svg, the observer
  * is reset to be nothing.
  *
  * NOTE: there maybe should be some sort of "drag end" event, so that things
  * can trigger when the drag is released.
  *
  * When something needs to be dragged, the state of the DragController is set
  * to be an observer for updates to the state of the thing being dragged.
  *
  * This depends on a MouseController.
  */

object DragController {
  case class State(
      observer: Observer[Complex],
      callback: Unit => Unit
  )

  def apply(mouse: MouseController) = {
    new DragController(Var(None), mouse)
  }
}

case class DragController(
    $state: Var[Option[DragController.State]],
    mouse: MouseController
) extends Modifier[SvgElement] {

  import DragController._

  /** This is a helper method that is used to process mouse events.
    */
  def processEvent(
      state: Option[State],
      evt: MouseEvent
  ): Option[State] = {
    import MouseEvent._

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

  def drag[Model](updates: Observer[Complex]): Action[Model, Unit] =
    Kleisli(_ =>
      IO.async_(cb => {
        $state.set(Some(State(updates, _ => cb(Right(())))))
      })
    )

  def dragStart[Model](updates: Observer[Complex]): Action[Model, Unit] =
    Kleisli(_ => IO($state.set(Some(State(updates, _ => ())))))

  /** This is used to attach the mouse move event handlers to the top-level SVG.
    */
  override def apply(el: SvgElement) = el.amend(
    mouse.mouseEvents --> $state.updater(processEvent)
  )
}
