package semagrams

import com.raquo.laminar.api.L._
import org.scalajs.dom

case class GlobalState(
    modifiers: Set[KeyModifier]
) {
  def processEvent(evt: Event) = evt match {
    case Event.KeyDown(key) => KeyModifier.fromString.get(key) match {
      case Some(mod) => GlobalState(modifiers + mod)
      case None => this
    }
    case Event.KeyUp(key) => KeyModifier.fromString.get(key) match {
      case Some(mod) => GlobalState(modifiers - mod)
      case None => this
    }
    case _ => this
  }
}

object GlobalState {
  def listen(into: Observer[Event]) = {
    dom.document.onkeydown = ev => into.onNext(KeyDown(ev.key))
    dom.document.onkeyup = ev => into.onNext(KeyUp(ev.key))
  }
}
