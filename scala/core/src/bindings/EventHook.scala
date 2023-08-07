package semagrams.bindings

import semagrams._

/** A trait for filters on the event stream, picking out events that are
  * relevant to a particular [[Action]] and extracting data of type `A` from
  * them.
  */
trait EventHook[A] {

  /** Determine whether or not this is triggered, and if it is triggered
    */
  def apply(evt: Event): Option[A]

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
  def apply(evt: Event) = evt match {
    case KeyDown(`key`) => Some(())
    case _              => None
  }

  def description = key
}

/** An [[EventHook]] for click events on entities.
  *
  * @param button
  *   the mouse button that we are listening for
  */
case class ClickOnEntityHook(button: MouseButton) extends EventHook[Entity] {
  def apply(evt: Event) = evt match {
    case MouseDown(Some(ent), `button`) => Some(ent)
    case _                              => None
  }

  def description = s"$button down"
}
