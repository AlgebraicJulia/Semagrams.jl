package semagrams.bindings

import semagrams._
import semagrams.acsets._

/** A trait for filters on the event stream, picking out events that are
  * relevant to a particular [[Action]] and extracting data of type `A` from
  * them.
  */
trait EventHook[A] {

  /** Determine whether or not this is triggered, and if it is triggered
    */
  def apply(evt: Event, globalState: GlobalState): Option[A]

  /** A brief description of what conditions trigger the eventhook, for use in
    * auto-generated help messages.
    */
  def description: String
}

/** An [[EventHook]] for keydown events.
  *
  * @param key
  *   the key that we are listening for
  */
case class KeyDownHook(key: String) extends EventHook[Unit] {
  def apply(evt: Event, _globalState: GlobalState) = evt match {
    case KeyDown(`key`) => Some(())
    case _              => None
  }

  def description = key
}

/** An [[EventHook]] for click events on parts.
  *
  * @param button
  *   the mouse button that we are listening for
  */
case class ClickOnPartHook(button: MouseButton, modifiers: Set[KeyModifier])
    extends EventHook[Part] {
  def apply(evt: Event, globalState: GlobalState) = evt match {
    case MouseDown(Some(ent: Part), `button`) if modifiers == globalState.modifiers =>
      Some(ent)
    case _                                    => None
  }

  def description = s"$button down on part"
}

object ClickOnPartHook {
  def apply(button: MouseButton): ClickOnPartHook = ClickOnPartHook(button, Set())
}



case class MsgHook[Model]() extends EventHook[Message[Model]] {

  def apply(evt: Event, globalState: GlobalState) = evt match {
    case MsgEvent[Model](msg) => Some(msg)
    case _ => None
  }

  def description = "receive a message to process"

}