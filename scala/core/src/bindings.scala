package semagrams

import com.raquo.laminar.api.L._
import cats.effect._
import semagrams.acsets._

type BindingPredicate = (EditorState, Event) => Boolean

/** A Binding associates an IO returning `A` action to events.
  *
  * @param selector
  *   The function which says if an event "matches", and if so, what to do with
  *   it.
  *
  * @param modifiers
  *   If present, the event only matches if these [[KeyModifier]]s are down.
  *
  * @param docs
  *   A description of the behavior of the binding, to be shown in a help
  *   screen.
  *
  * @tparam A
  *   The type that the binding eventually returns
  *
  * @todo
  *   selector should be concrete data that implements a trait, and part of that
  *   trait is a description of the binding, like "Ctrl-click on a box" or
  *   "press a".
  *
  * @todo
  *   actually use docs
  */
case class Binding[A](
    selector: PartialFunction[Event, IO[A]],
    // modifiers: Option[Set[KeyModifier]],
    predicate: Option[BindingPredicate],
    docs: String
) {

  /** Add another action to do after a match, using the result of the match. */
  def flatMap[B](g: A => IO[B]): Binding[B] =
    Binding(selector.andThen(_.flatMap(g)), predicate)

  /** Transform the result of the binding with `g` after a match. */
  def map[B](g: A => B): Binding[B] =
    Binding(selector.andThen(_.map(g)), predicate)

  /** Always return `b` after a match. */
  def mapTo[B](b: B): Binding[B] =
    Binding(selector.andThen(_.map(_ => b)), predicate)

  /** Always do `mb` after a match. */
  def andThen[B](mb: IO[B]) = Binding(selector.andThen(_ => mb), predicate)

  /** Raise an error after a match. */
  def fail(err: Error) =
    Binding[Unit](
      selector.andThen(_ => IO.raiseError(err)),
      predicate
    )

  /** Only match when a given predicate is satisfied. */
  def withPredicate(newPredicate: BindingPredicate) =
    Binding(selector, Some(newPredicate))

  /** Only match when `newModifiers` are set. */
  def withMods(newModifiers: KeyModifier*) =
    withPredicate(Binding.withModsPredicate(newModifiers.toSet))

  /** Only match when one of `newAltModifiers` are set. */
  def withAltMods(newAltModifiers: Set[KeyModifier]*) =
    withPredicate(Binding.withAltModsPredicate(newAltModifiers.toSet))

}

object Binding {

  /** A predicate that matches when `mods` are set */
  val withModsPredicate = (mods: Set[KeyModifier]) =>
    (es: EditorState, evt: Event) =>
      val currentMods = es.keyboard.keyState.now().modifiers
      currentMods == mods

  /** A predicate that matches when one of `alternativeMods` are set */
  val withAltModsPredicate = (altMods: Set[Set[KeyModifier]]) =>
    (es: EditorState, evt: Event) => {
      val currentMods = es.keyboard.keyState.now().modifiers
      altMods.exists((mods: Set[KeyModifier]) => currentMods == mods)
    }

  /** Construct a [[Binding]] with no modifiers and empty docs. */
  def apply[A](f: PartialFunction[Event, IO[A]]) =
    new Binding[A](f, None, "")

  /** Construct a [[Binding]] with `modifiers` and empty docs. */
  def apply[A](
      f: PartialFunction[Event, IO[A]],
      // modifiers: Option[Set[KeyModifier]]
      predicate: Option[BindingPredicate]
  ) = new Binding[A](f, predicate, "")
}

/** Matches events equal to `ev` */
def bindEvent(ev: Event) = Binding({ case `ev` => IO(()) })

/** Matches keydown events for `key` */
def keyDown(key: String) = bindEvent(KeyDown(key))

/** Matches keyup events for `key` */
def keyUp(key: String) = bindEvent(KeyUp(key))

/** Matches clicks on entities with `button`, returns the entity clicked on.
  */
def clickOn(button: MouseButton) = Binding(
  { case MouseDown(Some(ent), `button`) =>
    IO(ent)
  }
)

/** Matches clicks on entities of entity type `ty` with button `button`, returns
  * the entity clicked on.
  */
def clickOn[E <: Entity](button: MouseButton, ty: EntityType) = Binding(
  {
    case MouseDown(Some(ent), `button`) if ent.ty == ty =>
      IO(ent.asInstanceOf[E])
    case MouseDown(Some(ent), `button`)
        if (ty == ROOT.ty && ent == Background()) =>
      IO(ROOT.asInstanceOf[E])
  }
)

/** Matches clicks on parts of part type `ty` with button `button`, returns the
  * part clicked on.
  */
def clickOnPart(button: MouseButton, ty: PartType) = Binding(
  {
    case MouseDown(Some(i: Part), `button`) if i.ty == ty => IO(i)
    case MouseDown(Some(i: Part), `button`)
        if (ty == ROOT.ty && i == Background()) =>
      IO(ROOT)
  }
)

/** Matches clicks on parts with button `button`, returns the part clicked on.
  */
def clickOnPart(button: MouseButton) =
  Binding({ case MouseDown(Some(ent), `button`) =>
    ent match
      case Background() => IO(ROOT)
      case p: Part      => IO(p)

  })

/** [[clickOn]] for doubleclicks */
def dblClickOn(button: MouseButton) = Binding(
  { case DoubleClick(Some(ent), `button`) =>
    IO(ent)
  }
)

/** [[clickOnPart]] for doubleclicks */
def dblClickOnPart(button: MouseButton, ty: PartType) = Binding(
  {
    case DoubleClick(Some(i: Part), `button`) if i.ty == ty         => IO(i)
    case DoubleClick(Some(Background()), `button`) if ty == ROOT.ty => IO(ROOT)
  }
)

/** [[clickOnPart]] for doubleclicks */
def dblClickOnPart(button: MouseButton) =
  Binding({ case DoubleClick(Some(ent), `button`) =>
    ent match
      case Background() => IO(ROOT)
      case p: Part      => IO(p)
  })

/** Matches mouseup events on that are on an entity, returns that entity */
def releaseOn(button: MouseButton) = Binding(
  { case MouseUp(Some(ent), `button`) =>
    IO(ent)
  }
)

/** Matches a mouseup event either on or off an entity, returns an option of an
  * entity.
  */
def mouseUp(button: MouseButton) = Binding(
  { case MouseUp(e, `button`) =>
    IO(e)
  }
)

/** Matches a [[ContextMenu]] event on an entity, returns that entity. */
def menuOn() = Binding(
  { case ContextMenu(Some(ent)) =>
    IO(ent)
  }
)

/** Matches a [[ContextMenu]] event on a part of any type, returns that part.
  */
def menuOnPart() = Binding(
  { case ContextMenu(Some(i: Part)) =>
    IO(i)
  }
)

/** Matches a [[ContextMenu]] event on a part of a specific type, returns that
  * part.
  */
def menuOnPart(ty: PartType) = Binding(
  {
    case ContextMenu(Some(i: Part)) if i.ty == ty =>
      IO(i)
  }
)
