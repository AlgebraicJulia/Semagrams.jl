package semagrams.bindings

import semagrams._
import semagrams.acsets.abstr._


/** A trait for filters on the event stream, picking out events that are
  * relevant to a particular [[Action]] and extracting data of type `A` from
  * them.
  */
trait EventHook[A] {

  // override def toString = description

  /** Determine whether or not this is triggered, and if it is triggered
    */
  def apply(evt: Event, editorState: EditorState): Option[A]

  /** A brief description of what conditions trigger the eventhook, for use in
    * auto-generated help messages.
    */
  def description: String

  def filter(pred:A=>Boolean) = FilterMap(this,a => if pred(a) then Some(a) else None)
  def map[B](f:A => B) = FilterMap(this,a => Some(f(a)))
  def mapTo[B](b: () => B) = map(_ => b())
  def mapToValue[B](b:B) = map(_ => b)
  def collect[B](f:A => Option[B]) = FilterMap(this,f)
  def andThen(effect:A => Unit) = FilterMap(this, a =>
    effect(a)
    Some(a)
  )
}

case class FilterMap[A,B](base:EventHook[A],f:A=>Option[B]) extends EventHook[B] {
  def apply(evt:Event,es:EditorState) = 
    base.apply(evt,es).flatMap(f)
  val description = s"Optionally map an `EventHook[A]` by f:A => Option[B]"
}

case class MapHook[A,B](base:EventHook[A],f:A => B) extends EventHook[B] {
  def apply(evt:Event,es:EditorState) = base.apply(evt,es).map(f)
  val description = "Filter an `EventHook` by some property of its return value"
}


/** An [[EventHook]] for keydown events.
  *
  * @param key
  *   the key that we are listening for
  */
case class KeyDownHook(key: String) extends EventHook[Unit] {
  def apply(evt: Event, es: EditorState) = evt match 
    case KeyDown(`key`) => Some(())
    case _              => None


  def description = key
}

/** An [[EventHook]] for click events on parts.
  *
  * @param button
  *   the mouse button that we are listening for
  */
case class ClickOnPartHook(button: MouseButton, modifiers: Set[KeyModifier])
    extends EventHook[Part] {
  def apply(evt: Event, es: EditorState) = evt match {
    case MouseDown(Some(ent: Part), `button`) if modifiers == es.modifiers =>
      Some(ent)
    case _                                    => None
  }

  def description = s"$button down on part"
}

object ClickOnPartHook {
  def apply(button: MouseButton): ClickOnPartHook = ClickOnPartHook(button, Set())
  def apply(button: MouseButton,mod:KeyModifier): ClickOnPartHook = ClickOnPartHook(button, Set(mod))
  def apply(ob:Ob,button: MouseButton,mod:KeyModifier): EventHook[Part] = ClickOnPartHook(button, Set(mod)).filter(ob)
}


extension (hook:EventHook[Part])
  def filter(obs:Ob*) = hook.filter(part => obs.contains(part.ty))

case class DoubleClickOnPartHook(button: MouseButton = MouseButton.Left, modifiers: Set[KeyModifier] = Set()) extends EventHook[Part]:

  def apply(evt:Event, es: EditorState) = evt match {
    case DoubleClick(part,button) if modifiers == es.modifiers =>
      part
    case _ => None 
  }

  def description = s"double-click on part with $button"



case class MsgHook[Model]() extends EventHook[Message[Model]] {


  def apply(evt: Event, es: EditorState) = evt match {
    case MsgEvent(msg) => Some(msg.asInstanceOf[Message[Model]])
    case _ => None
  }

  def description = "receive a message to process"

}

