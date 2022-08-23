package semagrams.controllers

import semagrams._
import semagrams.util._

import com.raquo.laminar.api.L._
import monocle.std.these
import com.raquo.airstream.state.ObservedSignal

/**
 * This is meant to be a bit of global state that keeps track of
 * the current object being dragged. The drag handler has to go on
 * the main window, because it is easy for something that is being
 * dragged to not be under the mouse.
 *
 * The state of the drag controller is an option of observer of positions.
 * When this state is non-None, mouse move events get their position
 * sent to the observer. On mouse-up or when the mouse leaves the parent svg,
 * the observer is reset to be nothing.
 *
 * NOTE: there maybe should be some sort of "drag end" event, so that things
 * can trigger when the drag is released.
 *
 * When something needs to be dragged, the state of the DragController is
 * set to be an observer for updates to the state of the thing being dragged.
 *
 * This depends on a MouseController.
 */

case class DragController(
  $state: Var[Option[Observer[Complex]]],
  mouse: MouseController
) extends Modifier[SvgElement] {

  /**
   * This is a helper method that is used to process mouse events.
   */
  def processEvent(
    state: Option[Observer[Complex]],
    evt: MouseEvent
  ): Option[Observer[Complex]] = {
    import MouseEvent._
    state.flatMap(h =>
      evt match {
        case MouseMove(pos) => {
          h.onNext(pos)
          Some(h)
        }
        case MouseUp(_, _) => {
          None
        }
        case MouseLeave(_) => {
          None
        }
        case _ => {
          Some(h)
        }
      })
  }

  /**
   * This is used to make an element draggable.
   */
  def draggable[El <: Element](
    center: Signal[Complex],
    updates: Observer[Complex]
  ) = CustomModifier[El](
    el => {
      val curCenter = Var(Complex(0,0))
      el.amend(
        center --> curCenter.writer,
        onMouseDown.map(
          ev => {
            val c = curCenter.now()
            val init = mouse.$state.now().pos
            val offset = c - init
            Some(updates.contramap[Complex](offset + _))
          }) --> $state.writer
      )
    })

  /**
   * This is used to attach the mouse move event handlers to the
   * top-level SVG.
   */
  override def apply(el: SvgElement) = el.amend(
    mouse.mouseEvents --> $state.updater(processEvent)
  )
}

object DragController {
  def apply(mouse: MouseController) = {
    new DragController(Var(None), mouse)
  }
}
