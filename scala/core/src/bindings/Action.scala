package semagrams.bindings

import semagrams._
import semagrams.util._
import semagrams.state._
import semagrams.acsets._

import com.raquo.laminar.api.L._

import cats._
import cats.effect._
import cats.effect.std._
import scala.annotation.targetName
// import semagrams.{Ob, PropMap, PartProp}




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

  @targetName("andThenUnit")
  def andThen(post:Param => Unit): Action[Param,Model] =
    andThen(param => IO(post(param)))

  @targetName("andThenNoInput")
  def andThen(post:() => Unit): Action[Param,Model] =
    andThen(_ => IO(post()))
  
}

object Action {
  case class Resources[Model](
      modelVar: WeakVar[Model],
      stateVar: Var[EditorState],
      eventQueue: Queue[IO, Event],
      outbox: Observer[StateMsg[Model]]
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

case class AddAtMouse[D:PartData](getPartData: IO[Option[(Ob,D)]],select:Boolean = true) extends Action[Unit, ACSet[D]]:
  def apply(_p: Unit, r: Action.Resources[ACSet[D]]) = r.mousePos match
    case Some(z) => getPartData.map(_ match
      case Some((ob,data)) => r.modelVar.update(acset =>
        val (next,newPart) = acset.addPart(ob, data.setProp(Center,z))
        if select then r.stateVar.update(_.copy(selected = Seq(newPart)))
        next
      )
      case None => ()
    )
    case None => IO(())
    
  

  def description = s"add a new part at current mouse position"

object AddAtMouse:

  def apply[D:PartData](ob:Ob) = new AddAtMouse[D](IO(Some(ob -> PartData())))
  def apply[D:PartData](ob:Ob,init:D) = new AddAtMouse[D](IO(Some(ob -> init)))
  def apply[D:PartData](ob:Ob,initIO:IO[D]) = new AddAtMouse[D](initIO.map(init => Some(ob -> init)))


case class DeleteHovered[D:PartData](cascade:Boolean = true) extends Action[Unit, ACSet[D]] {
  def apply(_p: Unit, r: Action.Resources[ACSet[D]]) = IO(
    {
      r.stateVar
        .now()
        .hovered
        .map(_ match {
          case (i: Part) => {
            r.stateVar.update(_.copy(hovered = None))
            r.modelVar.update(m =>
              m.remPart(i,cascade))
          }
          case _ => 
            ()
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

case class MoveViaDrag[D:PartData]() extends Action[Part, ACSet[D]] {
  def apply(p: Part, r: Action.Resources[ACSet[D]]): IO[Unit] = 
    if r.modelVar.now().tryProp(Center,p).isEmpty
    then IO(())
    else for {
      ctr <- IO(r.modelVar.now().tryProp(Center,p))
      offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().getProp(Center, p))
      _ <- IO(r.modelVar.save())
      _ <- IO(r.modelVar.unrecord())
      _ <- IO(r.modelVar.update(_.moveToFront(p)))
      _ <- takeUntil(r.eventQueue)(
        evt => evt match {
          case Event.MouseMove(pos) => IO {
            r.modelVar.update(_.setProp(Center, p, pos - offset))
            None
          }
          case Event.MouseUp(_, _) => IO(Some(()))
          case Event.MouseLeave(backgroundPart) => IO(Some(()))
          case _                   => IO(None)
        }
      )
      _ <- IO(r.modelVar.record())
    } yield ()

  def description = "move part by dragging"
}




case class AddEdgeViaDrag[D:PartData](
  dummyData: Ob => IO[Option[(Ob,PartProp)]],
  tgtIO: (Ob,Ob) => IO[Option[(Ob,PartProp,PartProp,D)]]
) extends Action[Part, ACSet[D]]:
  def apply(srcPart: Part, r: Action.Resources[ACSet[D]]): IO[Unit] =
    dummyData(srcPart.ob).flatMap( dummyOpt =>
      dummyOpt.zip(r.mousePos).map{ case ((dummyOb,dummySrc),z0) => 
        val dummyData = PartData[D]().setProps(PropMap() +
          (dummySrc -> srcPart) + (End -> z0) + (Interactable -> false)
        )
        
        for 
          _ <- IO(r.modelVar.unrecord())
          _ = println("recording stopped")
          dummy <- IO {
            val (a,b) = r.modelVar.now().addPart(dummyOb,dummyData)
            r.modelVar.set(a)
            b
          }
          /* Drag loop */
          _ <- takeUntil(r.eventQueue)( evt => 
            r.processEvent(evt) >> (evt match {
              /* During drag */
              case Event.MouseMove(pos) => IO {
                r.modelVar.update(_.setProp(End, dummy, pos))
                None
              }
              /* End of drag: Good drop target */
              case Event.MouseUp(Some(tgtPart:Part), _) => 
                val dragIO = tgtIO(srcPart.ob,tgtPart.ob)
                for
                  dragOpt <- dragIO
                
                  _ <- dragOpt match
                    /* Good return from dragIO */
                    case Some((dragOb,dragSrc,dragTgt,data)) =>
                      if dragOb == dummyOb & dragSrc == dummySrc
                      then IO(r.modelVar.update(acset =>
                        acset.remProps(dummy,Seq(End,Interactable)).mergeData(dummy,data.setProp(dragTgt,tgtPart))
                      ))
                      else IO(r.modelVar.update(acset =>
                        acset.remPart(dummy).addPart(dragOb,
                          data.setProps(PropMap() + (dragSrc -> srcPart) + (dragTgt -> tgtPart))
                      )._1))
                    /* Bad return from dragIO */
                    case None => IO(r.modelVar.update(
                      _.remPart(dummy)
                    ))
                yield Some(())
              /* End of drag: Bad drop target */
              case Event.MouseUp(ent,but) => IO{
                r.modelVar.update(a => a.remPart(dummy))
                Some(())
              }
              /* Ignore other events */
              case _ => IO(None)

            })
          )
          _ <- IO(r.modelVar.record())
          _ = println("recording started")
        yield ()
      }.getOrElse(IO(()))
    )


  def description = "add edge by dragging from source to target"

object AddEdgeViaDrag:
  import PartData.propsAreData

  def apply[D:PartData](tgtObs:(Ob,Ob),edge:(Ob,PartProp,PartProp),init:D = PartData()): AddEdgeViaDrag[D] = 
    new AddEdgeViaDrag[D](
      ob => if ob == tgtObs._1 then IO(Some(edge._1 -> edge._2)) else IO(None),
      (src,tgt) => if ((src,tgt) == tgtObs) then IO(Some(edge :* init)) else IO(None)
    )


  
  def apply[D:PartData](tgts:((Ob,Ob),(Ob,PartProp,PartProp,D))*): AddEdgeViaDrag[D] = 
    new AddEdgeViaDrag[D](
      ob0 => IO(tgts.collect{ case tgt if tgt._1._1 == ob0 => tgt }
        .headOption.map{
          case _ -> (eob,esrc,_,_) => eob -> esrc
        }
      ),
      (src,tgt) => IO(tgts.find(_._1 == (src,tgt)).map(_._2))
    )



case class Callback[X,D:PartData](cb:X => Unit) extends Action[X,ACSet[D]]:

  def apply(x:X,r: Action.Resources[ACSet[D]]): IO[Unit] = 
    IO(cb(x))
  def description = "execute a callback function"

object Callback:
  def make(cb:() => Unit) = new Callback(_ => cb())

  def apply[X,D:PartData](cb:() => Unit) = new Callback[X,D](_ => cb())


case class PrintModel[Model](hoverRequired:Boolean=true) extends Action[Unit,Model]:
  def apply(u:Unit,r:Action.Resources[Model]) = 
    if hoverRequired & r.stateVar.now().hovered.isEmpty
    then IO(())
    else
      val acset = r.modelVar.now()
      val state = r.stateVar.now()
      IO {
        println(acset.toString())
        println(state.toString())
      }
  def description = "print a value to the console"


case class Die[X,Model]() extends Action[X,Model]:
  def apply(x:X,r:Action.Resources[Model]) = IO(())
  def description = "no op"