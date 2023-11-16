package semagrams.bindings

import acsets._
import balloons._
import semagrams._
import semagrams.util._
import com.raquo.laminar.api.L._
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.std._

/** A trait for actions which perform some effect on the Semagram. Actions are
  * paired with [[EventHook]]s in [[Binding]]s, and can use the data extracted
  * from the event in the [[Binding]].
  */
trait Action extends Described {
  def apply(
      res: Action.Resources
  ): IO[Option[Balloon[LocalEvent, Instance.Patch]]]
}

trait PureAction extends Described {
  def makePatch(
      editorState: EditorState,
      clean: Instance.Clean
  ): Instance.Patch = {
    val state = Instance.Dirty(clean, Instance.Patch.empty(clean.schema))
    mutateState(editorState, state)
    state.patch
  }

  def mutateState(editorState: EditorState, state: Instance.Dirty): Unit
}

object Action {
  case class Resources(
      instanceTether: Tether[Instance.Patch, Instance.Clean],
      globalTether: Tether[GlobalEvent, GlobalState],
      editorTether: Tether[EditorState.Event, EditorState]
  )

  /** A constructor for anonymous actions.
    */
  // def apply(
  //     f: Resources => IO[Unit],
  //     desc: String
  // ) =
  //   new Action[Param, Edit, Model] {
  //     def apply(args: Arguments[Edit, Model]) = f(p, args)
  //
  //     def description = desc
  //   }
}

case class AddAtMouse(sort: SortId) extends PureAction {
  def mutateState(editorState: EditorState, state: Instance.Dirty) = {
    val x = state.addPart(sort)
    state.setSubpart(x, Position, editorState.mousePos)
  }

  val description = s"add a new part of type $sort at current mouse position"
}

// case class DeleteHovered() extends Action[Unit, ACSet] {
//   def apply(_p: Unit, r: Action.Resources[ACSet]) = IO(
//     {
//       r.stateVar
//         .now()
//         .hovered
//         .map(_ match {
//           case (i: Part) => {
//             r.stateVar.update(_.copy(hovered = None))
//             r.modelVar.update(_.remPart(i))
//           }
//           case _ => ()
//         })
//         .getOrElse(())
//     }
//   )

//   def description = "remove hovered part"
// }

// def takeUntil[A,B](eventQueue: Queue[IO, A])(f: A => IO[Option[B]]): IO[B] = for {
//   a <- eventQueue.take
//   mb <- f(a)
//   b <- mb match {
//     case Some(b) => IO(b)
//     case None => takeUntil[A,B](eventQueue)(f)
//   }
// } yield b

// case class MoveViaDrag() extends Action[Part, ACSet] {
//   def apply(p: Part, r: Action.Resources[ACSet]): IO[Unit] = for {
//     offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().subpart(Center, p))
//     _ <- takeUntil(r.eventQueue)(
//       evt => evt match {
//         case Event.MouseMove(pos) =>
//           IO(r.modelVar.update(_.setSubpart(p, Center, pos - offset))) >> IO(None)
//         case Event.MouseUp(_, _) => IO(Some(()))
//         case _                   => IO(None)
//       })
//   } yield ()

//   def description = "move part by dragging"
// }

// case class AddEdgeViaDrag(ob: Ob, src: Hom, tgt: Hom) extends Action[Part, ACSet] {
//   def apply(p: Part, r: Action.Resources[ACSet]): IO[Unit] = for {
//     initpos <- IO(r.stateVar.now().mousePos)
//     e <- r.modelVar.updateS(
//       ACSet.addPart(ob, PropMap().set(src, p).set(End, initpos).set(Interactable, false)))
//     _ <- takeUntil(r.eventQueue)(
//       evt => r.processEvent(evt) >> (evt match {
//         case Event.MouseMove(pos) => r.modelVar.updateS_(ACSet.setSubpart(e, End, pos)) >> IO(None)
//         case Event.MouseUp(Some(q: Part), _) if tgt.codoms.contains(q.ty) =>
//           r.modelVar.updateS_(
//             ACSet.remSubpart(e, End)
//               >> ACSet.setSubpart(e, tgt, q)
//               >> ACSet.setSubpart(e, Interactable, true)
//           ) >> IO(Some(()))
//         case Event.MouseUp(_, _) => r.modelVar.updateS_(ACSet.remPart(e)) >> IO(Some(()))
//         case _ => IO(None)
//       })
//     )
//   } yield ()

//   def description = "add edge by dragging from source to target"
// }
