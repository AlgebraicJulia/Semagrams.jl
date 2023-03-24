package semagrams

import com.raquo.laminar.api.L._
import cats.effect._
import semagrams.acsets._

case class Binding[A](
    selector: PartialFunction[Event, IO[A]],
    modifiers: Option[Set[KeyModifier]],
    docs: String
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
    new Binding[A](f, None, "")

  def apply[A](
      f: PartialFunction[Event, IO[A]],
      modifiers: Option[Set[KeyModifier]]
  ) = new Binding[A](f, modifiers, "")
}

def bindEvent(ev: Event) = Binding({ case `ev` => IO(()) })

def keyDown(key: String) = bindEvent(KeyDown(key))

def keyUp(key: String) = bindEvent(KeyUp(key))

def clickOn(button: MouseButton) = Binding(
  { case MouseDown(Some(ent), `button`) =>
    IO(ent)
  }
)

def clickOn[E <: Entity](button: MouseButton, ty: EntityType) = Binding(
  {
    case MouseDown(Some(ent), `button`) if ent.ty == ty =>
      IO(ent.asInstanceOf[E])
    case MouseDown(Some(ent), `button`)
        if (ty == ROOT.ty && ent == Background()) =>
      IO(ROOT.asInstanceOf[E])
  }
)

def clickOnPart(button: MouseButton, ty: PartType) = Binding(
  {
    case MouseDown(Some(i: Part), `button`) if i.ty == ty => IO(i)
    case MouseDown(Some(i: Part), `button`)
        if (ty == ROOT.ty && i == Background()) =>
      IO(ROOT)
  }
)

def clickOnPart(button: MouseButton) =
  Binding({ case MouseDown(Some(ent), `button`) =>
    ent match
      case Background() => IO(ROOT)
      case p: Part      => IO(p)

  })

def dblClickOn(button: MouseButton) = Binding(
  { case DoubleClick(Some(ent), `button`) =>
    IO(ent)
  }
)

def dblClickOnPart(button: MouseButton, ty: PartType) = Binding(
  {
    case DoubleClick(Some(i: Part), `button`) if i.ty == ty         => IO(i)
    case DoubleClick(Some(Background()), `button`) if ty == ROOT.ty => IO(ROOT)
    // case DoubleClick(x,b) =>
    //   println(s"unknown $x, $b")
    //   IO(ROOT)
  }
)

def dblClickOnPart(button: MouseButton) =
  Binding({ case DoubleClick(Some(ent), `button`) =>
    ent match
      case Background() => IO(ROOT)
      case p: Part      => IO(p)
  })

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

def menuOn() = Binding(
  { case ContextMenu(Some(ent)) =>
    IO(ent)
  }
)

def menuOnPart(ty: PartType) = Binding(
  {
    case ContextMenu(Some(i: Part)) if i.ty == ty =>
      IO(i)
  }
)
