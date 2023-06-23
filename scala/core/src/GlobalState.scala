package semagrams

import com.raquo.laminar.api.L._

case class GlobalState(
  modifiers: Set[KeyModifier]
)

object GlobalState {
  def listen(into: Observer[Event]) = {
    windowEvents(_.onKeyDown.filter(ev => !ev.repeat)).map(ev => into.onNext(KeyDown(ev.key)))
    windowEvents(_.onKeyUp.filter(ev => !ev.repeat)).map(ev => into.onNext(KeyUp(ev.key)))
  }
}
