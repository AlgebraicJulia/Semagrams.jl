package semagrams.controllers

import com.raquo.laminar.api.L._
import org.scalajs.dom.raw.KeyboardEvent

/** Like the MouseController, this both provides a subscription to keyboard
  * events and also keeps track of the current state of the keyboard itself.
 */

enum KeyModifier:
  case Ctrl
  case Shift
  case Meta
  case Alt

  def isSet(evt: KeyboardEvent) = this match {
    case Ctrl => evt.ctrlKey
    case Shift => evt.shiftKey
    case Meta => evt.metaKey
    case Alt => evt.altKey
  }

object KeyModifier {
  val all = Set[KeyModifier](Ctrl, Shift, Meta, Alt)
}


/** This is a data structure representing the current state of the keyboard.
  */
case class KeyState(
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

object KeyState {
  def apply() = {
    new KeyState(Set[String](), Set[KeyModifier]())
  }
}

/** This attaches the right listeners to the toplevel SVG
  */
case class KeyboardController(
    keydowns: EventBus[KeyboardEvent],
    keyups: EventBus[KeyboardEvent],
    keyState: Var[KeyState]
) extends Modifier[SvgElement] {
  override def apply(el: SvgElement) = {
    el.amend(
      onKeyDown.stopPropagation.filter(ev => !ev.repeat) --> keydowns,
      onKeyUp.stopPropagation --> keyups,
      keydowns --> keyState.updater[KeyboardEvent]((state, evt) =>
        state.keydown(evt)
      ),
      keyups --> keyState.updater[KeyboardEvent]((state, evt) =>
        state.keyup(evt)
      )
    )
  }
}

object KeyboardController {
  def apply() = {
    new KeyboardController(
      EventBus[KeyboardEvent](),
      EventBus[KeyboardEvent](),
      Var(KeyState())
    )
  }
}
