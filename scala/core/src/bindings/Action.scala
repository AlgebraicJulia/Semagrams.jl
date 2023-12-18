package semagrams.bindings

import semagrams._
import semagrams.util._
import semagrams.state._
import semagrams.acsets._
import semagrams.rendering._
import semagrams.partprops._

import com.raquo.laminar.api.L._

import cats._
import cats.effect._
import cats.effect.std._
import scala.annotation.targetName

/** A trait for actions which perform some effect on the Semagram. Actions are
  * paired with [[EventHook]]s in [[Binding]]s, and can use the data extracted
  * from the event in the [[Binding]].
  */
trait Action[Param, Model] { self =>
  def apply(p: Param, r: Action.Resources[Model]): IO[Unit]

  def description: String

  def andThen(post: Param => IO[Unit]) = new Action[Param, Model] {
    def apply(p: Param, r: Action.Resources[Model]): IO[Unit] = for
      _ <- self.apply(p, r)
      _ <- post(p)
    yield ()

    def description = "add a postaction"
  }

  @targetName("andThenUnit")
  def andThen(post: Param => Unit): Action[Param, Model] =
    andThen(param => IO(post(param)))

  @targetName("andThenNoInput")
  def andThen(post: () => Unit): Action[Param, Model] =
    andThen(_ => IO(post()))

}

object Action {
  case class Resources[Model](
      modelVar: WeakVar[Model],
      stateVar: Var[EditorState],
      eventQueue: Queue[IO, Event],
      outbox: Observer[StateMsg[Model]]
  ) {
    def processEvent(evt: Event): IO[Unit] = IO(stateVar.update(editorState =>
      val msg = editorState.eventMsg(evt)
      outbox.onNext(Right(msg))
      msg.execute(editorState)
    ))

    def mousePos: Option[Complex] =
      stateVar.now().hovered.map(_ => stateVar.now().mousePos)

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

case class AddAtMouse(
    getProps: IO[Option[(Ob, PropMap)]],
    contextId: UID
) extends Action[Unit, ACSet]:
  def apply(_p: Unit, r: Action.Resources[ACSet]) = r.mousePos match
    case Some(z) =>
      getProps.map(_ match
        case Some((ob, data)) =>
          r.modelVar.update(acset =>
            val (next, newPart) = acset.addPart(ob, data.set(Center, z))
            r.stateVar.update(_.copy(selected = Seq(ObTag(newPart, contextId))))
            next
          )
        case None => ()
      )
    case None => IO(())

  def description = s"add a new part at current mouse position"

object AddAtMouse:

  def apply(ob: Ob, ctxt: UID) =
    new AddAtMouse(IO(Some(ob -> PropMap())), ctxt)
  def apply(ob: Ob, init: PropMap, ctxt: UID) =
    new AddAtMouse(IO(Some(ob -> init)), ctxt)
  def apply(ob: Ob, initIO: IO[PropMap], ctxt: UID) =
    new AddAtMouse(initIO.map(init => Some(ob -> init)), ctxt)

case class DeleteHovered(cascade: Boolean = true) extends Action[Unit, ACSet] {
  def apply(_p: Unit, r: Action.Resources[ACSet]) = IO(
    {
      r.stateVar
        .now()
        .hoveredPart
        .map { tag =>
          r.stateVar.update(_.copy(hovered = None))
          r.modelVar.update(m =>
            tag match
              case _: (ObTag | SpanTag) => m.remPart(tag.keyPart, cascade)
              case f: HomTag            => m.remProp(f.hom, f.keyPart)
          )
        }
        .getOrElse(())
    }
  )

  def description = "remove hovered part"
}

def takeUntil[A, B](eventQueue: Queue[IO, A])(f: A => IO[Option[B]]): IO[B] =
  for {
    a <- eventQueue.take
    mb <- f(a)
    b <- mb match {
      case Some(b) => IO(b)
      case None    => takeUntil[A, B](eventQueue)(f)
    }
  } yield b

case class MoveViaDrag() extends Action[PartTag, ACSet] {
  def apply(tag: PartTag, r: Action.Resources[ACSet]): IO[Unit] = tag match
    case tag: ObTag =>
      val p = tag.keyPart
      if r.modelVar.now().tryProp(Center, p).isEmpty
      then IO(())
      else
        for {
          ctr <- IO(r.modelVar.now().tryProp(Center, p))
          offset <- IO(
            r.stateVar.now().mousePos - r.modelVar.now().getProp(Center, p)
          )
          _ <- IO(r.modelVar.save())
          _ <- IO(r.modelVar.unrecord())
          _ <- IO(r.modelVar.update(_.moveToFront(p)))
          _ <- takeUntil(r.eventQueue)(evt =>
            evt match {
              case Event.MouseMove(pos) =>
                IO {
                  r.modelVar.update(_.setProp(Center, p, pos - offset))
                  None
                }
              case Event.MouseUp(_, _)              => IO(Some(()))
              case Event.MouseLeave(backgroundPart) => IO(Some(()))
              case _                                => IO(None)
            }
          )
          _ <- IO(r.modelVar.record())
        } yield ()
    case _ => IO(())

  def description = "move part by dragging"
}

case class AddSpanViaDrag(
    dummyData: Ob => IO[Option[PartHom]],
    tgtIO: (Ob, Ob) => IO[Option[(PartSpan, PropMap)]]
) extends Action[PartTag, ACSet]:
  def apply(src: PartTag, r: Action.Resources[ACSet]): IO[Unit] = src match
    case src: ObTag =>
      val srcPart = src.keyPart
      dummyData(srcPart.ob).flatMap(dummyOpt =>
        dummyOpt
          .zip(r.mousePos)
          .map { case (dummyf, z0) =>
            val dummyData = PropMap() +
              (dummyf -> srcPart) + (End -> z0) + (Interactable -> false)

            for
              _ <- IO(r.modelVar.unrecord())
              dummy <- IO {
                val (a, b) = r.modelVar.now().addPart(dummyf.dom, dummyData)
                r.modelVar.set(a)
                b
              }
              /* Drag loop */
              _ <- takeUntil(r.eventQueue)(evt =>
                r.processEvent(evt) >> (evt match {
                  /* During drag */
                  case Event.MouseMove(pos) =>
                    IO {
                      r.modelVar.update(_.setProp(End, dummy, pos))
                      None
                    }
                  /* End of drag: Good drop target */
                  case Event.MouseUp(Some(tgt: ObTag), _) =>
                    val tgtPart = tgt.keyPart
                    val dragIO = tgtIO(srcPart.ob, tgtPart.ob)
                    for
                      dragOpt <- dragIO

                      _ <- dragOpt match
                        /* Good return from dragIO */
                        case Some(span -> data) =>
                          if span.left == dummyf
                          then
                            IO(
                              r.modelVar.update(acset =>
                                acset
                                  .remProps(dummy, Seq(End, Interactable))
                                  .setProps(
                                    dummy,
                                    data.set(span.right, tgtPart)
                                  )
                              )
                            )
                          else
                            IO(
                              r.modelVar.update(acset =>
                                acset
                                  .remPart(dummy)
                                  .addPart(
                                    span.dom,
                                    data
                                      .set(span.left, srcPart)
                                      .set(span.right, tgtPart)
                                  )
                                  ._1
                              )
                            )
                        /* Bad return from dragIO */
                        case None =>
                          IO(
                            r.modelVar.update(
                              _.remPart(dummy)
                            )
                          )
                    yield Some(())
                  /* End of drag: Bad drop target */
                  case Event.MouseUp(ent, but) =>
                    IO {
                      r.modelVar.update(a => a.remPart(dummy))
                      Some(())
                    }
                  /* Ignore other events */
                  case _ => IO(None)

                })
              )
              _ <- IO(r.modelVar.record())
            yield ()
          }
          .getOrElse(IO(()))
      )
    case _ => IO(())
  def description = "add edge by dragging from source to target"

object AddSpanViaDrag:

  def apply(
      tgtObs: (Ob, Ob),
      edge: (PartHom, PartHom),
      init: PropMap = PropMap()
  ): AddSpanViaDrag =
    new AddSpanViaDrag(
      ob => if ob == tgtObs._1 then IO(Some(edge._1)) else IO(None),
      (src, tgt) =>
        if ((src, tgt) == tgtObs)
        then IO(Some(Span(edge._1, edge._2) -> init))
        else IO(None)
    )

  def apply(
      tgts: ((Ob, Ob), (PartHom, PartHom, PropMap))*
  ): AddSpanViaDrag =
    new AddSpanViaDrag(
      ob0 =>
        IO(tgts.collect { case tgt if tgt._1._1 == ob0 => tgt }.headOption.map {
          case _ -> (esrc, _, _) => esrc
        }),
      (src, tgt) =>
        IO(tgts.find(_._1 == (src, tgt)).map { case (_, (f, g, init)) =>
          Span(f, g) -> init
        })
    )

case class AddHomViaDrag(
    tgtIO: (Ob, Ob) => IO[Option[PartHom]]
) extends Action[PartTag, ACSet]:
  def apply(src: PartTag, r: Action.Resources[ACSet]): IO[Unit] = src match
    case src: ObTag =>
      val srcPart = src.keyPart
      for
        _ <- IO(r.modelVar.unrecord())
        _ <- r.mousePos match
          case None => IO(())
          case Some(z) => {
            for _ <- IO(r.modelVar.update(_.setProp(End, srcPart, z)))
            yield ()
          }
        /* Drag loop */

        _ <- takeUntil(r.eventQueue)(evt =>
          r.processEvent(evt) >> (evt match {
            /* During drag */
            case Event.MouseMove(pos) =>
              IO {
                r.modelVar.update(_.setProp(End, srcPart, pos))
                None
              }
            /* End of drag: Good drop target */
            case Event.MouseUp(Some(tgt: ObTag), _) =>
              val dragIO = tgtIO(srcPart.ob, tgt.ob)
              for
                dragOpt <- dragIO

                _ <- dragOpt match
                  /* Good return from dragIO */
                  case Some(f) =>
                    IO(
                      r.modelVar.update(acset =>
                        acset
                          .remProp(End, srcPart)
                          .setProp(f, srcPart, tgt.keyPart)
                      )
                    )
                  /* Bad return from dragIO */
                  case None =>
                    IO(r.modelVar.update(acset => acset.remProp(End, srcPart)))
              yield Some(())
            /* End of drag: Bad drop target */
            case Event.MouseUp(ent, but) =>
              IO {
                r.modelVar.update(a => a.remProp(End, srcPart))
                Some(())
              }
            /* Ignore other events */
            case _ => IO(None)

          })
        )
        _ <- IO(r.modelVar.record())
      yield ()
    case _ => IO(())

  def description = "add edge by dragging from source to target"

case class Callback[X](cb: X => Unit) extends Action[X, ACSet]:

  def apply(x: X, r: Action.Resources[ACSet]): IO[Unit] =
    IO(cb(x))
  def description = "execute a callback function"

object Callback:
  def make(cb: () => Unit) = new Callback(_ => cb())

  def apply[X](cb: () => Unit) =
    new Callback[X](_ => cb())

case class PrintModel[Model](hoverRequired: Boolean = true)
    extends Action[Unit, Model]:
  def apply(u: Unit, r: Action.Resources[Model]) =
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

case class Die[X, Model]() extends Action[X, Model]:
  def apply(x: X, r: Action.Resources[Model]) = IO(())
  def description = "no op"
