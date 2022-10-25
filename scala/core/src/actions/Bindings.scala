package semagrams.actions

import cats._
import cats.data._
import cats.effect._
import org.scalajs.dom.KeyboardEvent
import com.raquo.laminar.api.L._
import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.util._
import semagrams.widgets._

import Action.ops

case class Binding[Model, A](
    f: PartialFunction[Any, Action[Model, A]],
    modifiers: Option[Set[KeyModifier]]
) {
  def flatMap[B](g: A => Action[Model, B]): Binding[Model, B] =
    Binding(f.andThen(_.flatMap(g)), modifiers)

  def map[B](g: A => B): Binding[Model, B] =
    Binding(f.andThen(_.map(g)), modifiers)

  def mapTo[B](b: B): Binding[Model, B] =
    Binding(f.andThen(_.map(_ => b)), modifiers)

  def andThen[B](mb: Action[Model, B]) = Binding(f.andThen(_ => mb), modifiers)

  def fail =
    Binding[Model, Unit](
      f.andThen(_ => Action(_ => IO.raiseError(NoneError))),
      modifiers
    )

  def withMods(newModifiers: KeyModifier*) =
    Binding(f, Some(newModifiers.toSet))
}

object Binding {
  def apply[Model, A](f: PartialFunction[Any, Action[Model, A]]) =
    new Binding[Model, A](f, None)

  def apply[Model, A](
      f: PartialFunction[Any, Action[Model, A]],
      modifiers: Option[Set[KeyModifier]]
  ) = new Binding[Model, A](f, modifiers)
}

def keyDown[Model, A](key: String): Binding[Model, Unit] = Binding({
  case (evt: KeyboardEvent) if (evt.key == key && evt.`type` == "keydown") =>
    ops.pure(())
})

def keyUp[Model, A](key: String): Binding[Model, Unit] = Binding({
  case (evt: KeyboardEvent) if (evt.key == key && evt.`type` == "keyup") =>
    ops.pure(())
})

enum ClickType {
  case Single
  case Double
}

import ClickType._

def clickOn[Model](
    clickType: ClickType,
    button: MouseButton,
    x: EntityType
): Binding[Model, Entity] = Binding(
  {
    case MouseEvent.MouseDown(Some(ent), `button`)
        if (clickType == Single && ent.entityType == x) =>
      ops.pure(ent)
    case MouseEvent.DoubleClick(Some(ent), `button`)
        if (clickType == Double && ent.entityType == x) =>
      ops.pure(ent)
  },
  Some(Set())
)

def clickOnPart[Model](
    clickType: ClickType,
    button: MouseButton
): Binding[Model, Part] = Binding(
  {
    case MouseEvent.MouseDown(Some(ent: Part), `button`)
        if (clickType == Single) =>
      ops.pure(ent)
    case MouseEvent.DoubleClick(Some(ent: Part), `button`)
        if (clickType == Double) =>
      ops.pure(ent)
  },
  Some(Set())
)

def mouseDown[Model](b: MouseButton): Binding[Model, Option[Entity]] = Binding(
  {
    case MouseEvent.MouseDown(ent, `b`)  => ops.pure(ent)
  }
)

def releaseOn[Model](
    clickType: ClickType,
    button: MouseButton,
    x: Ob
): Binding[Model, Entity] = Binding(
  {
    case MouseEvent.MouseUp(Some(ent), `button`) if (ent.entityType == x) =>
      ops.pure(ent)
  },
  Some(Set())
)

def mouseMove[Model] = Binding[Model, Complex]({ case MouseEvent.MouseMove(p) =>
  ops.pure(p)
})

def mouseUp[Model](button: MouseButton) = Binding[Model, Option[Entity]]({
  case MouseEvent.MouseUp(e, `button`) =>
    ops.pure(e)
})

def mouseLeave[Model] = Binding[Model, Complex]({
  case MouseEvent.MouseLeave(p) =>
    ops.pure(p)
})

class Bindings[Model, A](bindings: Seq[Binding[Model, A]]) {

  /** This listens for a keybinding, and then executes the action associated
    * with that keybinding. Crucially, this only runs *once*. If you want to run
    * this in a loop, you should provide that loop yourself.
    */
  def runNoCatch: Action[Model, A] =
    for {
      bindables <- ops[Model].ask.map(_.bindables)
      keyState <- ops[Model].ask.map(_.keyboard.keyState)
      nextAction <- nextEvent(bindables.events.collect(((ev: Any) => {
        bindings.collectFirst(
          (
              (bnd: Binding[Model, A]) =>
                bnd.modifiers match {
                  case Some(mods) =>
                    if (keyState.now().modifiers == mods) {
                      bnd.f.lift(ev)
                    } else {
                      None
                    }
                  case None => bnd.f.lift(ev)
                }
          ).unlift
        )
      }).unlift))
      res <- nextAction
    } yield res

  def run: Action[Model, Option[A]] =
    Action(es => runNoCatch(es).map(Some(_)).handleError(_ => None))

  def runUntilFail: Action[Model, Unit] =
    Action(es => ops.foreverM(runNoCatch)(es).handleError(_ => ()))

  def runForever: Action[Model, Unit] = ops.foreverM(run)
}

object Bindings {
  def apply[Model, A](bindings: Binding[Model, A]*) = new Bindings(
    Seq(bindings*)
  )
}

def showPopoverUntil[Model](
    lines: Seq[String],
    binding: Binding[Model, Unit]
): Action[Model, Unit] = {
  for {
    eltDims <- ops[Model].ask.map(_.dims())
    popover <- ops.delay(Popover(Val(lines), 400, 15, 15, eltDims))
    h <- addControlElt(popover)
    _ <- Bindings(binding).run
    _ <- removeControlElt(h)
  } yield ()
}
