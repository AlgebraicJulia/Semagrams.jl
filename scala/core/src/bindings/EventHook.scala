package semagrams.bindings

import semagrams._
import semagrams.state._
import semagrams.acsets.{Ob, Elt}
import semagrams.rendering._
import semagrams.partprops._

/** A trait for filters on the event stream, picking out events that are
  * relevant to a particular [[Action]] and extracting data of type `A` from
  * them.
  */
trait EventHook[A] {

  /** Determine whether or not this is triggered, and if it is triggered
    */
  def apply(evt: Event, editorState: EditorState): Option[A]

  /** A brief description of what conditions trigger the eventhook, for use in
    * auto-generated help messages.
    */
  def description: String

  def filter(pred: A => Boolean) =
    FilterMap(this, a => if pred(a) then Some(a) else None)
  def filter(test: () => Boolean) =
    FilterMap(this, a => if test() then Some(a) else None)
  def map[B](f: A => B) = FilterMap(this, a => Some(f(a)))
  def mapTo[B](b: () => B) = map(_ => b())
  def mapToValue[B](b: B) = map(_ => b)
  def collect[B](f: PartialFunction[A, B]) = FilterMap(this, f.lift)
  def andThen(effect: A => Unit) = FilterMap(
    this,
    a =>
      effect(a)
      Some(a)
  )
}

case class FilterMap[A, B](base: EventHook[A], f: A => Option[B])
    extends EventHook[B] {
  def apply(evt: Event, es: EditorState) =
    base.apply(evt, es).flatMap(f)
  val description = s"Optionally map an `EventHook[A]` by f:A => Option[B]"
}

/** An [[EventHook]] for keydown events.
  *
  * @param key
  *   the key that we are listening for
  */
case class KeyDownHook(key: String, mods: Option[Set[KeyModifier]] = None)
    extends EventHook[Unit] {
  def apply(evt: Event, es: EditorState) = evt match
    case KeyDown(`key`) if (mods == None | mods == Some(es.modifiers)) =>
      Some(())
    case _ =>
      None

  def description = key
}

object KeyDownHook:
  def apply(key: String, mods: KeyModifier*) = new KeyDownHook(
    key,
    mods match
      case Seq() => None
      case _     => Some(mods.toSet)
  )

/** An [[EventHook]] for click events on parts.
  *
  * @param button
  *   the mouse button that we are listening for
  */
case class ClickOnPartHook(button: MouseButton, modifiers: Set[KeyModifier])
    extends EventHook[PartTag] {
  def apply(evt: Event, es: EditorState) = evt match {
    case MouseDown(Some(part: PartTag), `button`)
        if modifiers == es.modifiers =>
      Some(part)
    case _ => None
  }

  def description = s"$button down on part"
}

object ClickOnPartHook {
  def apply(button: MouseButton): ClickOnPartHook =
    ClickOnPartHook(button, Set())
  def apply(button: MouseButton, mod: KeyModifier): ClickOnPartHook =
    ClickOnPartHook(button, Set(mod))
  def apply(ob: Ob, button: MouseButton, mod: KeyModifier): EventHook[PartTag] =
    ClickOnPartHook(button, Set(mod)).filter(ob)
}

extension (hook: EventHook[PartTag])
  def filter(elts: Elt*) = hook.collect {
    case tag: ObTag if elts.contains(tag.ob)    => tag
    case tag: SpanTag if elts.contains(tag.dom) => tag
    case tag: HomTag if elts.contains(tag.hom)  => tag
  }

case class DoubleClickOnPartHook(
    button: MouseButton = MouseButton.Left,
    modifiers: Set[KeyModifier] = Set()
) extends EventHook[PartTag]:

  def apply(evt: Event, es: EditorState) = evt match {
    case DoubleClick(Some(part: PartTag), button)
        if modifiers == es.modifiers =>
      Some(part)
    case _ => None
  }

  def description = s"double-click on part with $button"
