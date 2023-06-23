package semagrams

import semagrams.acsets._
import cats.effect._
import cats._

trait EventHook[A] {
  def apply(evt: Event): Option[A]

  def description: String
}

case class KeyDownHook(key: String) extends EventHook[Unit] {
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

object Action {
  def apply[Param, Output](f: Param => IO[Output], desc: String) =
    new Action[Param, Output] {
      def apply(p: Param) = f(p)

      def description = desc
    }
}

trait Binding[X] {
  type EventData

  val hook: EventHook[EventData]

  val action: Action[EventData, X]
}

object Binding {
  def apply[A, B](h: EventHook[A], a: Action[A, B]): Binding[B] = new Binding[B] {
    type EventData = A
    val hook = h
    val action = a
  }

  def process(evt: Event, bindings: Seq[Binding[Unit]]): IO[Unit] =
    bindings
      .collectFirst(((b: Binding[Unit]) => b.hook(evt).map(b.action(_))).unlift)
      .getOrElse(IO(()))

}
