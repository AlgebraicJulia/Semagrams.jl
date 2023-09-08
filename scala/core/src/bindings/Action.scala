package semagrams.bindings

import semagrams._
import semagrams.util._
import semagrams.acsets._
import com.raquo.laminar.api.L._
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.std._
import upickle.default._

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
      globalStateVar: StrictSignal[GlobalState],
      eventQueue: Queue[IO, Event],
      outbox: Observer[Message[Model]]
  ) {
    def processEvent(evt: Event): IO[Unit] =
      IO(stateVar.update(_.processEvent(evt)))

  }

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
  def apply(_p: Unit, r: Action.Resources[ACSet]) = 
    r.stateVar.now().hovered match
    case Some(ent) => 
      val pos = r.stateVar.now().mousePos match
        case Complex(0,0) => r.stateVar.now().dims/2.0
        case z => z
      
      IO(r.modelVar.update(
        _.addPart(ob, PropMap() + (Center -> pos))._1
      ))
    
    case None => IO(())
  

  def description = s"add a new part of type $ob at current mouse position"
}

case class Add(ob: Ob,props:PropMap = PropMap()) extends Action[Unit, ACSet] {
  def apply(_p: Unit, r: Action.Resources[ACSet]) = IO(
    {
      r.modelVar.update(_.addPart(ob, props)._1)
    }
  )

  def description = s"add a new part of type $ob with $props"
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

def takeUntil[A,B](eventQueue: Queue[IO, A])(f: A => IO[Option[B]]): IO[B] = for {
  a <- eventQueue.take
  mb <- f(a)
  b <- mb match {
    case Some(b) => IO(b)
    case None => takeUntil[A,B](eventQueue)(f)
  }
} yield b

case class MoveViaDrag() extends Action[Part, ACSet] {
  def apply(p: Part, r: Action.Resources[ACSet]): IO[Unit] = for {
    offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().subpart(Center, p))
    _ <- takeUntil(r.eventQueue)(
      evt => evt match {
        case Event.MouseMove(pos) =>
          IO(r.modelVar.update(_.setSubpart(p, Center, pos - offset))) >> IO(None)
        case Event.MouseUp(_, _) => IO(Some(()))
        case Event.MouseLeave(Background()) => IO(Some(()))
        case _                   => IO(None)
      })
  } yield ()

  def description = "move part by dragging"
}

case class AddEdgeViaDrag(ob: Ob, src: Hom, tgt: Hom) extends Action[Part, ACSet] {
  def apply(p: Part, r: Action.Resources[ACSet]): IO[Unit] = for {
    initpos <- IO(r.stateVar.now().mousePos)
    e <- r.modelVar.updateS(
      ACSet.addPart(ob, PropMap().set(src, p).set(End, initpos).set(Interactable, false)))
    _ <- takeUntil(r.eventQueue)(
      evt => r.processEvent(evt) >> (evt match {
        case Event.MouseMove(pos) => r.modelVar.updateS_(ACSet.setSubpart(e, End, pos)) >> IO(None)
        case Event.MouseUp(Some(q: Part), _) if tgt.codoms.contains(q.ty) =>
          r.modelVar.updateS_(
            ACSet.remSubpart(e, End)
              >> ACSet.setSubpart(e, tgt, q)
              >> ACSet.setSubpart(e, Interactable, true)
          ) >> IO(Some(()))
        case Event.MouseUp(_, _) => r.modelVar.updateS_(ACSet.remPart(e)) >> IO(Some(()))
        case _ => IO(None)
      })
    )
  } yield ()

  def description = "add edge by dragging from source to target"
}

case class ProcessMsg[Model]() extends Action[Message[Model],Model] {

  def apply(msg:Message[Model],r: Action.Resources[Model]): IO[Unit] = IO(
    r.modelVar.update(msg.execute)
  )

  def description = "process a message from outside the semagram"



}

case class PartCallback(cb:Entity => Unit) extends Action[Entity,ACSet] {

  def apply(ent:Entity,r: Action.Resources[ACSet]): IO[Unit] = IO {
    cb(ent)
  }

  def description = "process a message from outside the semagram"



}

case object PrintModel extends Action[Unit,ACSet]:
  def apply(u:Unit,r:Action.Resources[ACSet]) = 
    val acset = r.modelVar.now()
    val gs = r.stateVar.now()
    val ogs = r.globalStateVar.now()
    IO {
      println(acset.toString())
      println(gs.toString())
      println(ogs.toString())
    }
  def description = "print a value to the console"


