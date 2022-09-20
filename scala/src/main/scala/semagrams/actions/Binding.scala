package semagrams.actions

import cats._
import cats.data._
import cats.effect._
import org.scalajs.dom.raw.KeyboardEvent
import semagrams.acsets._
import semagrams.controllers.MouseButton
import semagrams.controllers.MouseEvent

abstract class Binding[Model] {
  def tryMatch: PartialFunction[Any, Action[Model, Unit]]
}

case class KeyDown[Model](key: String, action: Action[Model, Unit])
    extends Binding[Model] {
  def tryMatch = {
    case (evt: KeyboardEvent) if (evt.key == key && evt.`type` == "keydown") =>
      action
  }
}

enum ClickType {
  case Single
  case Double
}

case class ClickOn[Model, X <: Ob](
    clickType: ClickType,
    button: MouseButton,
    x: X,
    action: Elt[X] => Action[Model, Unit]
) extends Binding[Model] {
  import ClickType._
  def tryMatch = {
    case MouseEvent.MouseDown(Some(ent), `button`)
        if (clickType == Single && ent.entityType == x) =>
      action(ent.asInstanceOf[Elt[X]])
    case MouseEvent.DoubleClick(Some(ent), `button`)
        if (clickType == Double && ent.entityType == x) =>
      action(ent.asInstanceOf[Elt[X]])
  }
}

class Bindings[Model](bindings: Seq[Binding[Model]]) {

  /** This listens for a keybinding, and then executes the action associated
    * with that keybinding. Crucially, this only runs *once*. If you want to run
    * this in a loop, you should provide that loop yourself.
    */
  def run: Action[Model, Unit] =
    for {
      bindables <- ReaderT.ask.map(_.bindables)
      nextAction <- nextEvent(bindables.events.collect(((ev: Any) => {
        bindings.collectFirst(
          ((bnd: Binding[Model]) => bnd.tryMatch.lift(ev)).unlift
        )
      }).unlift))
      _ <- nextAction
    } yield ()

  def runForever: Action[Model, Unit] = {
    val T = implicitly[Monad[Action[Model, _]]]
    T.foreverM(run)
  }
}

object Bindings {
  def apply[Model](bindings: Binding[Model]*) = new Bindings(Seq(bindings*))
}
