package semagrams.controllers

import semagrams._
import com.raquo.laminar.api.L._
import org.scalajs.dom.KeyboardEvent

/** Like the MouseController, this both provides a subscription to keyboard
  * events and also keeps track of the current state of the keyboard itself.
  */
class KeyboardController() extends Controller {
  import KeyboardController.State

  val keyState = Var(State())

  def apply(es: EditorState, el: SvgElement) = {
    val keydowns = EventBus[KeyboardEvent]()
    val keyups = EventBus[KeyboardEvent]()
    el.amend(
      onKeyDown.stopPropagation.filter(ev => !ev.repeat) --> keydowns,
      onKeyUp.stopPropagation --> keyups,
      keydowns --> keyState.updater[KeyboardEvent]((state, evt) =>
        state.keydown(evt)
      ),
      keyups --> keyState.updater[KeyboardEvent]((state, evt) =>
        state.keyup(evt)
      ),
      keydowns.events.map(ev => KeyDown(ev.key)) --> es.events,
      keyups.events.map(ev => KeyUp(ev.key)) --> es.events
    )
  }
}

object KeyboardController {
  def apply() = new KeyboardController()

  /** This is a data structure representing the current state of the keyboard.
    */
  case class State(
      keys: Set[String],
      modifiers: Set[KeyModifier]
  ) {
    def updateModifiers(evt: KeyboardEvent) = {
      this.copy(
        modifiers = KeyModifier.all.filter(_.isSet(evt))
      )
    }

    def keydown(evt: KeyboardEvent) = {
      updateModifiers(evt).copy(
        keys = keys + evt.key
      )
    }

    def keyup(evt: KeyboardEvent) = {
      updateModifiers(evt).copy(
        keys = keys - evt.key
      )
    }
  }

  object State {
    def apply() = {
      new State(Set[String](), Set[KeyModifier]())
    }
  }
}
