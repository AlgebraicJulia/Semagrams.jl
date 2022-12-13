package semagrams

import com.raquo.laminar.api.L._
import cats.effect._

case class Binding[A](
    selector: PartialFunction[Event, IO[A]],
    modifiers: Option[Set[KeyModifier]]
) {
  def flatMap[B](g: A => IO[B]): Binding[B] =
    Binding(selector.andThen(_.flatMap(g)), modifiers)

  def map[B](g: A => B): Binding[B] =
    Binding(selector.andThen(_.map(g)), modifiers)

  def mapTo[B](b: B): Binding[B] =
    Binding(selector.andThen(_.map(_ => b)), modifiers)

  def andThen[B](mb: IO[B]) = Binding(selector.andThen(_ => mb), modifiers)

  def fail(err: Error) =
    Binding[Unit](
      selector.andThen(_ => IO.raiseError(err)),
      modifiers
    )

  def withMods(newModifiers: KeyModifier*) =
    Binding(selector, Some(newModifiers.toSet))
}

object Binding {
  def apply[A](f: PartialFunction[Event, IO[A]]) =
    new Binding[A](f, None)

  def apply[A](
      f: PartialFunction[Event, IO[A]],
      modifiers: Option[Set[KeyModifier]]
  ) = new Binding[A](f, modifiers)
}

def bindEvent(ev: Event) = Binding({ case `ev` => IO(()) })

def keyDown(key: String) = bindEvent(KeyDown(key))

def keyUp(key: String) = bindEvent(KeyUp(key))

def clickOn(button: MouseButton) = Binding(
  { case MouseDown(Some(ent), `button`) =>
    IO(ent)
  }
)

def dblClickOn(button: MouseButton) = Binding(
  { case DoubleClick(Some(ent), `button`) =>
    IO(ent)
  }
)

def releaseOn(button: MouseButton) = Binding(
  { case MouseUp(Some(ent), `button`) =>
    IO(ent)
  }
)

def mouseUp(button: MouseButton) = Binding(
  { case MouseUp(e, `button`) =>
    IO(e)
  }
)
