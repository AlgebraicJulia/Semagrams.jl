package semagrams.bindings

import semagrams._
import semagrams.util._
// import semagrams.acsets._
import com.raquo.laminar.api.L._
import cats._
import cats.implicits._
import cats.effect._
import cats.effect.std._
import upickle.default._

import semagrams.acsets.abstr._
import scala.annotation.targetName
import cats.data.OptionT
// import semagrams.acsets.abstr.ACSet

import scala.concurrent.duration._

/** A trait for actions which perform some effect on the Semagram. Actions are
  * paired with [[EventHook]]s in [[Binding]]s, and can use the data extracted
  * from the event in the [[Binding]].
  */
trait Action[Param, Model] {self => 
  def apply(p: Param, r: Action.Resources[Model]): IO[Unit]

  def description: String

  def andThen(post:Param => IO[Unit]) = new Action[Param,Model] {
    def apply(p: Param, r:Action.Resources[Model]): IO[Unit] = for
      _ <- self.apply(p,r)
      _ <- post(p)
    yield ()

    def description = "add a postaction"    
  }


}

object Action {
  case class Resources[Model](
      modelVar: UndoableVar[Model],
      stateVar: Var[EditorState],
      // globalStateVar: StrictSignal[GlobalState],
      eventQueue: Queue[IO, Event],
      outbox: Observer[Either[Message[Model],Message[EditorState]]]
  ) {
    def processEvent(evt: Event): IO[Unit] = IO(stateVar.update( editorState =>
      val msg = editorState.eventMsg(evt)
      outbox.onNext(Right(msg))
      msg.execute(editorState)
    ))

    def mousePos: Option[Complex] = stateVar.now().hovered.map(_ =>
      stateVar.now().mousePos
    )


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

case class AddAtMouse[D:PartData,A:ACSetWithData[D]](getPartProps: IO[Option[(Ob,D)]],select:Boolean = true) extends Action[Unit, A] {
  def apply(_p: Unit, r: Action.Resources[A]) = r.mousePos match
    case Some(z) =>
      getPartProps.map( _ match
        case Some((ob,data)) => r.modelVar.update(acset =>
          val (next,newPart) = acset.addPart(ob, data.setProp(Center,z))
          if select then r.stateVar.update(_.copy(selected = Seq(newPart)))
          next
        )
        case None => 
          println("No part props")
          ()
      )
    case None =>
      println("No mousePos")
      IO(())
    
  

  def description = s"add a new part at current mouse position"
}
object AddAtMouse:

  /* Static objects */
  def apply[D:PartData,A:ACSetWithData[D]](ob:Ob): AddAtMouse[D,A] = 
    AddAtMouse[D,A](ob,PartData[D]())
  def apply[D:PartData,A:ACSetWithData[D]](ob:Ob,data:D): AddAtMouse[D,A] = 
    AddAtMouse[D,A](IO(ob -> data))
  def apply[D:PartData,A:ACSetWithData[D]](ob:Ob,dataIO:IO[D]): AddAtMouse[D,A] = 
    AddAtMouse[D,A](dataIO.map(ob -> _))

  /* Unfailing IO */
  def apply[D:PartData,A:ACSetWithData[D]](obIO:IO[Ob]): AddAtMouse[D,A] = 
    AddAtMouse[D,A](obIO.map(_ -> PartData[D]()))
  @targetName("AddAtMouseObData")
  def apply[D:PartData,A:ACSetWithData[D]](obDataIO:IO[(Ob,D)]): AddAtMouse[D,A] = 
    AddAtMouse[D,A](obDataIO.map(pair => Some(pair)))

  /* IO with failure */
  @targetName("AddAtMouseObOption")
  def apply[D:PartData,A:ACSetWithData[D]](optIO:IO[Option[Ob]]): AddAtMouse[D,A] = 
    AddAtMouse[D,A](optIO.map(_.map(_ -> PartData[D]())))


case class Add[A:ACSet](ob: Ob,props:PropMap = PropMap()) extends Action[Unit, A] {
  def apply(_p: Unit, r: Action.Resources[A]) = IO(
    {
      r.modelVar.update(_.addPart(ob, props)._1)
    }
  )

  def description = s"add a new part of type $ob with $props"
}

case class DeleteHovered[A:ACSet](cascade:Boolean = true) extends Action[Unit, A] {
  def apply(_p: Unit, r: Action.Resources[A]) = IO(
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

def takeUntil[A,B](eventQueue: Queue[IO, A])(f: A => IO[Option[B]]): IO[B] = 
  for {
  a <- eventQueue.take
  mb <- f(a)
  b <- mb match {
    case Some(b) => IO(b)
    case None => takeUntil[A,B](eventQueue)(f)
  }
} yield b

case class MoveViaDrag[A:ACSet]() extends Action[Part, A] {
  def apply(p: Part, r: Action.Resources[A]): IO[Unit] = 
    if r.modelVar.now().tryProp(Center,p).isEmpty
    then IO(())
    else for {
      ctr <- IO(r.modelVar.now().tryProp(Center,p))
      offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().getProp(Center, p))
      _ <- IO(r.modelVar.update(_.moveToFront(p)))
      _ <- takeUntil(r.eventQueue)(
        evt => evt match {
          case Event.MouseMove(pos) =>
            IO(r.modelVar.update(_.setProp(Center, p, pos - offset))) >> IO(None)
          case Event.MouseUp(_, _) => IO(Some(()))
          case Event.MouseLeave(backgroundPart) => IO(Some(()))
          case _                   => IO(None)
        })
    } yield ()

  def description = "move part by dragging"
}


case class AddEdgeViaDrag[A:ACSet](tgtObs: Map[Ob,(Ob,GenHom[_],GenHom[_])]) extends Action[Part, A] {
  def apply(srcPart: Part, r: Action.Resources[A]): IO[Unit] = 
    for {
        initpos <- IO(r.stateVar.now().mousePos)
        /* Create temporary part */
        (tempTgt,(tempOb,tempSrc,_)) = tgtObs.toSeq.head
        tempPart <- r.modelVar.updateIO(a => a.addPart(tempOb, 
          PropMap().set(tempSrc, srcPart).set(End, initpos).set(Interactable, false)
        ))
        /* Drag loop */
        _ <- takeUntil(r.eventQueue)(
          evt => r.processEvent(evt) >> (evt match {
            /* During drag */
            case Event.MouseMove(pos) => IO {
              r.modelVar.update(_.setProp(End, tempPart, pos))
              None
            }
            /* End of drag: Good drop target */
            case Event.MouseUp(Some(tgtPart:Part), _) if 
              tgtObs.keySet.contains(tgtPart.ob) => {
                val (dragOb,dragSrc,dragTgt) = tgtObs(tgtPart.ob)

                r.modelVar.update{ a =>
                  val a0 = a.remPart(tempPart)

                  if dragSrc.codom != srcPart.ty 
                    | dragTgt.codom != tgtPart.ty
                  then 
                    a0
                  else 

                    a0.addPart(dragOb,PropMap()
                    .set(dragSrc, srcPart).set(dragTgt, tgtPart)
                    .set(Interactable, true)
                  )._1
                }
                IO(Some(()))
              }
            /* End of drag: Bad drop target */
            case Event.MouseUp(ent,but) =>
              r.modelVar.update(a => a.remPart(tempPart))
              IO(Some(()))
            case _ => IO(None)
          })
        )
      } yield ()

  def description = "add edge by dragging from source to target"
}

object AddEdgeViaDrag:
  def apply[A:ACSet](e:Ob,src:GenHom[Ob],tgt:GenHom[Ob]): AddEdgeViaDrag[A] = 
    AddEdgeViaDrag(
      Map(tgt.codom -> (e,src,tgt))
    )
  def apply[A:ACSet](tgtObData:(Ob, (Ob,GenHom[Ob],GenHom[Ob]))*): AddEdgeViaDrag[A] = 
    AddEdgeViaDrag(tgtObData.toMap)


case class ProcessMsg[Model]() extends Action[Message[Model],Model] {

  def apply(msg:Message[Model],r: Action.Resources[Model]): IO[Unit] = IO(
    r.modelVar.update(msg.execute)
  )

  def description = "process a message from outside the semagram"



}




case class Callback[X,A:ACSet](cb:X => Unit) extends Action[X,A] {

  def apply(x:X,r: Action.Resources[A]): IO[Unit] = IO {
    cb(x)
  }

  def description = "execute a callback function"



}

case class PrintModel[Model](hoverRequired:Boolean=true) extends Action[Unit,Model]:
  def apply(u:Unit,r:Action.Resources[Model]) = 
    if hoverRequired & r.stateVar.now().hovered.isEmpty
    then IO(())
    else
      val acset = r.modelVar.now()
      val gs = r.stateVar.now()
      // val ogs = r.globalStateVar.now()
      IO {
        println(acset.toString())
        println(gs.toString())
        // println(ogs.toString())
      }
  def description = "print a value to the console"


case class Die[X,Model]() extends Action[X,Model]:
  def apply(x:X,r:Action.Resources[Model]) = IO(())
  def description = "no op"