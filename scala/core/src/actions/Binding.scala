package semagrams.actions

import cats._
import cats.data._
import cats.effect._
import org.scalajs.dom.raw.KeyboardEvent
import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.util._

case class Binding[Model, A](f: PartialFunction[Any, Action[Model, A]]) {
  def flatMap[B](g: A => Action[Model, B]): Binding[Model, B] =
    Binding(f.andThen(_.flatMap(g)))

  def map[B](g: A => B): Binding[Model, B] = Binding(f.andThen(_.map(g)))

  def mapTo[B](b: B): Binding[Model, B] = Binding(f.andThen(_.map(_ => b)))

  def andThen[B](mb: Action[Model, B]) = Binding(f.andThen(_ => mb))

  def fail =
    Binding[Model, Unit](f.andThen(_ => Kleisli(_ => IO.raiseError(NoneError))))
}

def keyDown[Model, A](key: String): Binding[Model, Unit] = Binding({
  case (evt: KeyboardEvent) if (evt.key == key && evt.`type` == "keydown") =>
    Kleisli.pure(())
})

def keyUp[Model, A](key: String): Binding[Model, Unit] = Binding({
  case (evt: KeyboardEvent) if (evt.key == key && evt.`type` == "keyup") =>
    Kleisli.pure(())
})

enum ClickType {
  case Single
  case Double
}

import ClickType._

def clickOn[Model, X <: Ob](
    clickType: ClickType,
    button: MouseButton,
    x: X
): Binding[Model, Elt[X]] = Binding({
  case MouseEvent.MouseDown(Some(ent), `button`)
      if (clickType == Single && ent.entityType == x) =>
    Kleisli.pure(ent.asInstanceOf[Elt[X]])
  case MouseEvent.DoubleClick(Some(ent), `button`)
      if (clickType == Double && ent.entityType == x) =>
    Kleisli.pure(ent.asInstanceOf[Elt[X]])
})

def releaseOn[Model, X <: Ob](
    clickType: ClickType,
    button: MouseButton,
    x: X
): Binding[Model, Elt[X]] = Binding({
  case MouseEvent.MouseUp(Some(ent), `button`) if (ent.entityType == x) =>
    Kleisli.pure(ent.asInstanceOf[Elt[X]])
})

def mouseMove[Model] = Binding[Model, Complex]({ case MouseEvent.MouseMove(p) =>
  Kleisli.pure(p)
})

def mouseUp[Model](button: MouseButton) = Binding[Model, Option[Entity]]({
  case MouseEvent.MouseUp(e, `button`) =>
    Kleisli.pure(e)
})

def mouseLeave[Model] = Binding[Model, Complex]({
  case MouseEvent.MouseLeave(p) =>
    Kleisli.pure(p)
})

class Bindings[Model, A](bindings: Seq[Binding[Model, A]]) {

  /** This listens for a keybinding, and then executes the action associated
    * with that keybinding. Crucially, this only runs *once*. If you want to run
    * this in a loop, you should provide that loop yourself.
    */
  def runNoCatch: Action[Model, A] =
    for {
      bindables <- ReaderT.ask.map(_.bindables)
      nextAction <- nextEvent(bindables.events.collect(((ev: Any) => {
        bindings.collectFirst(
          ((bnd: Binding[Model, A]) => bnd.f.lift(ev)).unlift
        )
      }).unlift))
      res <- nextAction
    } yield res

  def run: Action[Model, Option[A]] =
    Kleisli(es => runNoCatch(es).map(Some(_)).handleError(_ => None))

  def runUntilFail: Action[Model, Unit] = {
    val T = implicitly[Monad[Action[Model, _]]]
    Kleisli(es => T.foreverM(runNoCatch)(es).handleError(_ => ()))
  }

  def runForever: Action[Model, Unit] = {
    val T = implicitly[Monad[Action[Model, _]]]
    T.foreverM(run)
  }
}

object Bindings {
  def apply[Model, A](bindings: Binding[Model, A]*) = new Bindings(
    Seq(bindings*)
  )
}

def showPopoverUntil[Model](
    lines: Seq[String],
    binding: Binding[Model, Unit]
): Action[Model, Unit] = for {
  _ <- showPopover(lines*)
  _ <- Bindings(binding).run
  _ <- hidePopover
} yield ()
