package semagrams.controllers

import semagrams.util._
import com.raquo.laminar.api.L._
import scala.collection.immutable.BitSet

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

enum MouseEvent:
  case MouseDown(pos: Complex, button: MouseButton)
  case MouseUp(pos: Complex, button: MouseButton)
  case MouseLeave(pos: Complex)
  case MouseMove(pos: Complex)

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

case class MouseController(
  $state: Var[MouseState],
  mouseEvents: EventBus[MouseEvent]
) extends Modifier[SvgElement] {
  override def apply(el: SvgElement) = {
    import MouseEvent._
    el.amend(
      onMouseDown.map(ev => MouseDown(Complex(ev.clientX, ev.clientY), MouseButton.fromJS(ev.button))) --> mouseEvents,
      onMouseUp.map(ev => MouseUp(Complex(ev.clientX, ev.clientY), MouseButton.fromJS(ev.button))) --> mouseEvents,
      onMouseLeave.map(ev => MouseLeave(Complex(ev.clientX, ev.clientY))) --> mouseEvents,
      onMouseMove.map(ev => MouseMove(Complex(ev.clientX, ev.clientY))) --> mouseEvents,
      mouseEvents --> $state.updater[MouseEvent]((state, evt) => state.processEvent(evt))
    )
  }
}

object MouseController {
  def apply() = {
    new MouseController(Var(MouseState(Complex(0,0), BitSet())), EventBus[MouseEvent]())
  }
}
