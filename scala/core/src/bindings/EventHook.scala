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

  def filter(pred:A=>Boolean) = FilterHook(this,pred)

}


case class FilterHook[A](base:EventHook[A],pred:A=>Boolean) extends EventHook[A] {
  def apply(evt:Event,gs:GlobalState) = base.apply(evt,gs).filter(pred)
  val description = "Filter an `EventHook` by some property of its return value"
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


case class DoubleClickOnPartHook(button: MouseButton = MouseButton.Left, modifiers: Set[KeyModifier] = Set()) extends EventHook[Entity]:

  def apply(evt:Event, globalState: GlobalState) = evt match {
    case DoubleClick(ent,button) if modifiers == globalState.modifiers =>
      ent
    case _ => None 
  }

  def description = s"double-click on part with $button"



case class MsgHook[Model]() extends EventHook[Message[Model]] {


  def apply(evt: Event, globalState: GlobalState) = evt match {
    case MsgEvent(msg) => Some(msg.asInstanceOf[Message[Model]])
    case _ => None
  }

  def description = "receive a message to process"

}

