package semagrams.controllers

import semagrams.util._
import org.scalajs.dom
import com.raquo.laminar.api.L._
import scala.collection.immutable.BitSet
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent

/**
 * The MouseController provides two functionalities. One is to keep
 * track of the current position of the mouse, and the buttons pressed.
 * The other is to provide an event stream for mouse events on the global
 * window.
 */

/**
 * We provide our own enum for mouse buttons, so that we don't have to
 * remember JavaScript's
 */
enum MouseButton:
  case LeftButton
  case MiddleButton
  case RightButton

object MouseButton {
  def fromJS(idx: Int) = idx match {
    case 0 => LeftButton
    case 1 => MiddleButton
    case 2 => RightButton
  }
}

/**
 * We simplify the mouse API to just these events
 */
enum MouseEvent:
  case MouseDown(pos: Complex, button: MouseButton)
  case MouseUp(pos: Complex, button: MouseButton)
  case MouseLeave(pos: Complex)
  case MouseMove(pos: Complex)

object MouseEvent {
  def mouseDown(ev: TypedTargetMouseEvent[dom.Element]) =
    MouseDown(Complex(ev.clientX, ev.clientY), MouseButton.fromJS(ev.button))

  def mouseUp(ev: TypedTargetMouseEvent[dom.Element]) =
    MouseUp(Complex(ev.clientX, ev.clientY), MouseButton.fromJS(ev.button))

  def mouseLeave(ev: TypedTargetMouseEvent[dom.Element]) =
    MouseLeave(Complex(ev.clientX, ev.clientY))

  def mouseMove(ev: TypedTargetMouseEvent[dom.Element]) =
    MouseMove(Complex(ev.clientX, ev.clientY))
}

/**
 * The state of the mouse is simply its positions and
 * what buttons are currently pressed.
 *
 * It processes mouse events to keep this updated.
 */
case class MouseState(
  pos: Complex,
  pressed: BitSet
) {
  def processEvent(evt: MouseEvent): MouseState = {
    import MouseEvent._
    evt match {
      case MouseDown(pos, button) => this.copy(pos = pos, pressed = pressed + button.ordinal)
      case MouseUp(pos, button) => this.copy(pos = pos, pressed = pressed - button.ordinal)
      case MouseLeave(pos) => this.copy(pos = pos)
      case MouseMove(pos) => this.copy(pos = pos)
    }
  }
}

object MouseState {
  def apply() = new MouseState(Complex(0,0), BitSet())
}

case class MouseController(
  $state: Var[MouseState],
  mouseEvents: EventBus[MouseEvent]
) extends Modifier[SvgElement] {
  /**
   * This attaches the necessary event listeners to the main window
   */
  override def apply(el: SvgElement) = {
    import MouseEvent._
    el.amend(
      onMouseDown.map(MouseEvent.mouseDown) --> mouseEvents,
      onMouseUp.map(MouseEvent.mouseUp) --> mouseEvents,
      onMouseLeave.map(MouseEvent.mouseLeave) --> mouseEvents,
      onMouseMove.map(MouseEvent.mouseMove) --> mouseEvents,
      mouseEvents --> $state.updater[MouseEvent]((state, evt) => state.processEvent(evt))
    )
  }
}

object MouseController {
  def apply() = {
    new MouseController(Var(MouseState()), EventBus[MouseEvent]())
  }
}
