package semagrams.controllers

import semagrams._
import semagrams.util._
import org.scalajs.dom
import com.raquo.laminar.api.L._
import scala.collection.immutable.BitSet
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom

/** A bit of global state to keep track of the mouse, along with the code that
  * subscribes EditorState to mouse events.
  */

class MouseController() extends Controller {
  import MouseController.State
  val $state = Var(State())
  val mouseEvents = EventBus[Event]()

  def svgCoords(
      el: dom.SVGSVGElement,
      ev: dom.MouseEvent
  ): Complex = {
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

  def mouseMove(el: dom.SVGSVGElement)(
      ev: TypedTargetMouseEvent[dom.Element]
  ) =
    MouseMove(svgCoords(el, ev))

  /** Attaches the necessary event listeners to the main window
    */
  def apply(es: EditorState, el: SvgElement) = {
    val svgEl = el.ref.asInstanceOf[dom.SVGSVGElement]
    el.amend(
      onMouseLeave.map(mouseLeave(svgEl)) --> mouseEvents,
      onMouseMove.map(mouseMove(svgEl)) --> mouseEvents,
      mouseEvents --> $state.updater[Event]((state, evt) =>
        state.processEvent(evt)
      ),
      mouseEvents --> es.events,
      clickable(Background())
    )
  }

  /** Makes a certain SVG element record clicking
    *
    * @param ent
    *   the entity to attribute the clicks to
    */
  def clickable(ent: Entity) = List(
    onContextMenu.stopPropagation.preventDefault.map(evt =>
      ContextMenu(Some(ent))
    ) --> mouseEvents,
    onMouseDown.stopPropagation.map(evt =>
      MouseDown(Some(ent), MouseButton.fromJS(evt.button))
    ) --> mouseEvents,
    onMouseUp.stopPropagation.map(evt =>
      MouseUp(Some(ent), MouseButton.fromJS(evt.button))
    ) --> mouseEvents,
    onDblClick.stopPropagation.map(evt =>
      DoubleClick(Some(ent), MouseButton.fromJS(evt.button))
    ) --> mouseEvents
  )
}

object MouseController {
  def apply() = {
    new MouseController()
  }

  /** The state of the MouseController as a value
    *
    * @param pos
    *   the current position of the mouse
    * @param pressed
    *   the current buttons pressed
    */
  case class State(
      pos: Complex,
      pressed: BitSet
  ) {

    /** Update the state based on a mouse event */
    def processEvent(evt: Event): State = {
      evt match {
        case MouseDown(_, button) =>
          this.copy(pressed = pressed + button.ordinal)
        case MouseUp(_, button) =>
          this.copy(pressed = pressed - button.ordinal)
        case MouseLeave(pos) => this.copy(pos = pos)
        case MouseMove(pos)  => this.copy(pos = pos)
        case e               => this
      }
      
    }
  }

  object State {
    def apply() = new State(Complex(0, 0), BitSet())
  }
}
