package semagrams.controllers

import semagrams._
import semagrams.util._
import semagrams.actions._
import org.scalajs.dom
import com.raquo.laminar.api.L._
import scala.collection.immutable.BitSet
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom.SVGElement
import org.scalajs.dom.SVGSVGElement

/** The MouseController provides two functionalities. One is to keep track of
  * the current position of the mouse, and the buttons pressed. The other is to
  * provide an event stream for mouse events on the global window.
  */

/** We provide our own enum for mouse buttons, so that we don't have to remember
  * JavaScript's
  */
enum MouseButton:
  case Left
  case Middle
  case Right

object MouseButton {
  def fromJS(idx: Int) = idx match {
    case 0 => Left
    case 1 => Middle
    case 2 => Right
  }
}

/** We simplify the mouse API to just these events
  */
enum MouseEvent:
  case MouseDown(ent: Option[Entity], button: MouseButton)
  case MouseUp(ent: Option[Entity], button: MouseButton)
  case DoubleClick(ent: Option[Entity], button: MouseButton)
  case MouseLeave(pos: Complex)
  case MouseMove(pos: Complex)

object MouseEvent {
  def svgCoords(el: dom.SVGSVGElement, ev: dom.MouseEvent): Complex = {
    val pt = el.createSVGPoint()
    pt.x = ev.clientX
    pt.y = ev.clientY
    val svgP = pt.matrixTransform(el.getScreenCTM().inverse())
    Complex(svgP.x, svgP.y)
  }

  def mouseLeave(el: dom.SVGSVGElement)(
      ev: TypedTargetMouseEvent[dom.Element]
  ) =
    MouseLeave(svgCoords(el, ev))

  def mouseMove(el: dom.SVGSVGElement)(ev: TypedTargetMouseEvent[dom.Element]) =
    MouseMove(svgCoords(el, ev))
}

/** The state of the mouse is simply its positions and what buttons are
  * currently pressed.
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
      case MouseDown(pos, button) =>
        this.copy(pressed = pressed + button.ordinal)
      case MouseUp(pos, button) =>
        this.copy(pressed = pressed - button.ordinal)
      case MouseLeave(pos) => this.copy(pos = pos)
      case MouseMove(pos)  => this.copy(pos = pos)
      case _               => this
    }
  }
}

object MouseState {
  def apply() = new MouseState(Complex(0, 0), BitSet())
}

case class MouseController(
    $state: Var[MouseState],
    mouseEvents: EventBus[MouseEvent]
) extends Modifier[SvgElement] {

  /** This attaches the necessary event listeners to the main window
    */
  override def apply(el: SvgElement) = {
    import MouseEvent._
    val svgEl = el.ref.asInstanceOf[SVGSVGElement]
    el.amend(
      onMouseLeave.map(MouseEvent.mouseLeave(svgEl)) --> mouseEvents,
      onMouseMove.map(MouseEvent.mouseMove(svgEl)) --> mouseEvents,
      onMouseDown.map(evt =>
        MouseEvent.MouseDown(None, MouseButton.fromJS(evt.button))
      ) --> mouseEvents,
      onMouseUp.map(evt =>
        MouseEvent.MouseUp(None, MouseButton.fromJS(evt.button))
      ) --> mouseEvents,
      onDblClick.map(evt =>
        MouseEvent.DoubleClick(None, MouseButton.fromJS(evt.button))
      ) --> mouseEvents,
      mouseEvents --> $state.updater[MouseEvent]((state, evt) =>
        state.processEvent(evt)
      )
    )
  }

  /** This makes a certain SVG element record clicking
    */
  def clickable(ent: Entity) = List(
    onMouseDown.stopPropagation.map(evt =>
      MouseEvent.MouseDown(Some(ent), MouseButton.fromJS(evt.button))
    )
      --> mouseEvents,
    onMouseUp.stopPropagation.map(evt =>
      MouseEvent.MouseUp(Some(ent), MouseButton.fromJS(evt.button))
    )
      --> mouseEvents,
    onDblClick.stopPropagation.map(evt =>
      MouseEvent.DoubleClick(Some(ent), MouseButton.fromJS(evt.button))
    ) --> mouseEvents
  )
}

object MouseController {
  def apply() = {
    new MouseController(Var(MouseState()), EventBus[MouseEvent]())
  }
}
