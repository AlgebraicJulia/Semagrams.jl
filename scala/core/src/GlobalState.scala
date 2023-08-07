package semagrams

import com.raquo.laminar.api.L._
import org.scalajs.dom

case class GlobalState(
    modifiers: Set[KeyModifier]
)

object GlobalState {
  def listen(into: Observer[Event]) = {
    dom.document.onkeydown = ev => into.onNext(KeyDown(ev.key))
    dom.document.onkeyup = ev => into.onNext(KeyUp(ev.key))
  }
}
