package semagrams

import org.scalajs.dom.KeyboardEvent

enum KeyModifier:
  case Ctrl
  case Shift
  case Meta
  case Alt

  def isSet(evt: KeyboardEvent): Boolean =
    if (evt.key == KeyModifier.asString(this)) {
      evt.`type` == "keydown" || evt.`type` == "keypress"
    } else {
      this match {
        case Ctrl  => evt.ctrlKey
        case Shift => evt.shiftKey
        case Meta  => evt.metaKey
        case Alt   => evt.altKey
      }
    }

object KeyModifier {
  val all = Set[KeyModifier](Ctrl, Shift, Meta, Alt)

  val asString =
    Map(Ctrl -> "Ctrl", Shift -> "Shift", Meta -> "Meta", Alt -> "Alt")

  val fromString = asString.toList.map({ case (mod, str) => (str, mod) }).toMap
}
