package semagrams.controllers

import semagrams._
import semagrams.util._
import com.raquo.laminar.api.L._
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.console
import java.lang.reflect.Constructor

/** A bit of global state that keeps track of the keyboard.
  *
  * The main functions are:
  *   - Keeping track of what keys and modifiers are currently held down
  *   - Adding keyboard events to EditorState
  */
class KeyboardController() extends Controller {
  import KeyboardController.State

  val keyState = Var(State())

  /** Get the keypress events from `el` and hook them into updating the
    * KeyboardController state and additionally send them into `es.events`
    */
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

  /** The current state of the keyboard.
    *
    * @param keys
    *   the keys that are currently held down
    * @param modifiers
    *   the modifiers (i.e. shift, ctrl, etc.) that are currently held down
    */
  case class State(
      keys: Set[String],
      modifiers: Set[KeyModifier]
  ) {

    /** Update the state of the modifiers based on the keyboard event */
    def updateModifiers(evt: KeyboardEvent) = {
      this.copy(
        modifiers = KeyModifier.all.filter(_.isSet(evt))
      )
    }

    /** Update the state based on a keydown event */
    def keydown(evt: KeyboardEvent) = {
      updateModifiers(evt).copy(
        keys = keys + evt.key
      )
    }

    /** Update the state based on a keyup event */
    def keyup(evt: KeyboardEvent) = {
      updateModifiers(evt).copy(
        keys = keys - evt.key
      )
    }
  }

  object State {

    /** Construct a new [[State]] object that starts with no keys or modifiers
      * pressed
      */
    def apply() = {
      new State(Set[String](), Set[KeyModifier]())
    }
  }
}
