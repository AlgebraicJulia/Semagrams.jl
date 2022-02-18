package semagrams.controllers

import com.raquo.laminar.api.L._
import java.util.BitSet
import org.scalajs.dom.raw.KeyboardEvent
import cats.effect._
import cats.data._
import com.raquo.airstream.ownership.OneTimeOwner

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
      keys = keys + evt.key,
    )
  }

  def keyup(evt: KeyboardEvent) = {
    updateModifiers(evt).copy(
      keys = keys - evt.key,
    )
  }
}

object KeyState {
  def apply() = {
    new KeyState(Set[String](), false, false, false, false)
  }
}

case class KeyboardController(
  keydowns: EventBus[KeyboardEvent],
  keyups: EventBus[KeyboardEvent],
  keyState: Var[KeyState],
) extends Modifier[SvgElement] {
  override def apply(el: SvgElement) = {
    el.amend(
      onKeyDown.filter(ev => !ev.repeat) --> keydowns,
      onKeyUp --> keyups,
      keydowns --> keyState.updater[KeyboardEvent]((state, evt) => state.keydown(evt)),
      keyups --> keyState.updater[KeyboardEvent]((state, evt) => state.keyup(evt)),
    )
  }
}

object KeyboardController{
  def apply() = {
    new KeyboardController(EventBus[KeyboardEvent](), EventBus[KeyboardEvent](), Var(KeyState()))
  }
}
