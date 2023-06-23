package semagrams

import semagrams.acsets._
import cats.effect._

trait EventHook[A] {
  def apply(evt: Event): Option[A]

  def description: String
}

case class KeyboardHook(key: String) extends EventHook[Unit] {
  def apply(evt: Event) = evt match {
    case KeyDown(`key`) => Some(())
    case _ => None
  }

  def description = key
}

case class ClickOnEntityHook(button: MouseButton) extends EventHook[Entity] {
  def apply(evt: Event) = evt match {
    case MouseDown(Some(ent), `button`) => Some(ent)
    case _ => None
  }

  def description = s"$button down"
}

trait Action[Param, Output] {
  def apply(p: Param): IO[Output]

  def description: String
}

case class AddPartAction(ob: Ob) extends Action[Unit, Unit] {
  def apply(p: Unit) = IO(())

  def description = s"add $ob"
}

trait Binding[X] {
  type EventData

  val hook: EventHook[EventData]

  val action: Action[EventData, X]
}
