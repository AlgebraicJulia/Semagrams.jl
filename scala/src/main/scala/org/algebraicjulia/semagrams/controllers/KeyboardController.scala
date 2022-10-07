package org.algebraicjulia.semagrams.controllers

import com.raquo.laminar.api.L._
import org.scalajs.dom.raw.KeyboardEvent

/** Like the MouseController, this both provides a subscription to keyboard
  * events and also keeps track of the current state of the keyboard itself.
  */

/** This is a data structure representing the current state of the keyboard.
  */
case class KeyState(
    keys: Set[String],
    ctrl: Boolean,
    shift: Boolean,
    meta: Boolean,
    alt: Boolean
) {
  def updateModifiers(evt: KeyboardEvent) = {
    this.copy(
      ctrl = evt.ctrlKey,
      shift = evt.shiftKey,
      meta = evt.metaKey,
      alt = evt.altKey
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
    new KeyState(Set[String](), false, false, false, false)
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
