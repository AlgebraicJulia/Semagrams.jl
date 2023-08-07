package semagrams.bindings

import semagrams._
import semagrams.util._
import semagrams.acsets._
import com.raquo.laminar.api.L._
import cats.effect._
import cats.effect.std._

/** A trait for actions which perform some effect on the Semagram. Actions are
  * paired with [[EventHook]]s in [[Binding]]s, and can use the data extracted
  * from the event in the [[Binding]].
  */
trait Action[Param, Model] {
  def apply(p: Param, r: Action.Resources[Model]): IO[Unit]

  def description: String
}

object Action {
  case class Resources[Model](
      modelVar: UndoableVar[Model],
      stateVar: Var[EditorState],
      globalStateVar: Var[GlobalState],
      eventQueue: Queue[IO, Event]
  )

  /** A constructor for anonymous actions.
    */
  def apply[Param, Model](
      f: (Param, Resources[Model]) => IO[Unit],
      desc: String
  ) =
    new Action[Param, Model] {
      def apply(p: Param, r: Resources[Model]) = f(p, r)

      def description = desc
    }
}

case class AddAtMouse(ob: Ob) extends Action[Unit, ACSet] {
  def apply(_p: Unit, r: Action.Resources[ACSet]) = IO(
    {
      val pos = r.stateVar.now().mousePos
      r.modelVar.update(_.addPart(ob, PropMap() + (Center -> pos))._1)
    }
  )

  def description = s"add a new part of type $ob at current mouse position"
}

case class DeleteHovered() extends Action[Unit, ACSet] {
  def apply(_p: Unit, r: Action.Resources[ACSet]) = IO(
    {
      r.stateVar
        .now()
        .hovered
        .map(_ match {
          case (i: Part) => {
            r.stateVar.update(_.copy(hovered = None))
            r.modelVar.update(_.remPart(i))
          }
          case _ => ()
        })
        .getOrElse(())
    }
  )

  def description = "remove hovered part"
}

case class MoveViaDrag() extends Action[Entity, ACSet] {
  def apply(p: Entity, r: Action.Resources[ACSet]): IO[Unit] = for {
    evt <- r.eventQueue.take
    _ <- evt match {
      case Event.MouseMove(pos) =>
        for {
          _ <- IO(
            r.modelVar.update(_.setSubpart(p.asInstanceOf[Part], Center, pos))
          )
          _ <- apply(p, r)
        } yield ()
      case Event.MouseUp(_, _) => IO(())
      case _                   => apply(p, r)
    }
  } yield ()

  def description = "move part by dragging"
}
