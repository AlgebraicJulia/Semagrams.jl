package semagrams

import org.scalajs.dom.KeyboardEvent

enum KeyModifier:
  case Ctrl
  case Shift
  case Meta
  case Alt

  def isSet(evt: KeyboardEvent) = this match {
    case Ctrl  => evt.ctrlKey
    case Shift => evt.shiftKey
    case Meta  => evt.metaKey
    case Alt   => evt.altKey
  }

object KeyModifier {
  val all = Set[KeyModifier](Ctrl, Shift, Meta, Alt)
}
