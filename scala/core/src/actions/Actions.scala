package semagrams.actions

import cats._
import cats.data._
import cats.effect._
import cats.effect.syntax.all._
import cats.effect.unsafe.IORuntime
import cats.mtl.Local
import cats.syntax.all._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.nodes.ReactiveSvgElement
import com.raquo.airstream.core.Transaction
import com.raquo.laminar.CollectionCommand
import scala.scalajs.js

import semagrams._
import semagrams.controllers._
import semagrams.util._
import semagrams.widgets._

/** We control the behavior of Semagrams using an asynchronous IO monad from
  * cats-effect.
  *
  * This allows the "state" of Semagrams to simply be a continuation for
  * receiving the next event. This makes things like modal interfaces and
  * multi-keypress commands very easy; rather than explicitly representing the
  * state machine as an enum, we can simply write something like
  *
  * ```scala
  * for {
  *   source <- selectSource()
  *   target <- selectTarget()
  *   makeEdge(source, target)
  * } yield ()
  * ```
  *
  * where selectSource and selectTarget might involve arbitrary interaction of
  * the user with Semagrams.
  */
package object actions {}

type Action[Model, A] = ReaderT[IO, EditorState[Model], A]

extension [Model, A](action: Action[Model, A]) {
  def toOption: Action[Model, Option[A]] = {
    Action(es => action(es).map(Some(_)).handleError(_ => None))
  }

  def onCancelOrError(fin: Action[Model, A]) =
    Action[Model, A](es =>
      action(es).onCancel(fin(es).map(_ => ())).handleErrorWith(_ => fin(es))
    )
}

object Action {
  def apply[Model, A](f: EditorState[Model] => IO[A]): Action[Model, A] =
    Kleisli[IO, EditorState[Model], A](f)

  def ops[Model]: ActionOps[Model] = {
    new ActionOps[Model]()
  }
}

import Action.ops

class ActionOps[Model] {
  val asyncImpl = Async.asyncForKleisli[IO, EditorState[Model]]
  export asyncImpl.*
  val localImpl = Local.baseLocalForKleisli[IO, EditorState[Model]]
  export localImpl.{ask, local}
}

def runAction[Model](
    state: EditorState[Model],
    action: Action[Model, Unit]
): Unit = {
  action.run(state).unsafeRunAndForget()(IORuntime.global)
}

case object NoneError extends Exception

extension [A](x: Option[A]) {
  def unwrap[F[_]](implicit F: MonadError[F, Throwable]): F[A] = {
    x match {
      case Some(x) => F.pure(x)
      case None    => F.raiseError(NoneError)
    }
  }
}

def actionMonadError[Model]: MonadError[Action[Model, _], Throwable] =
  Kleisli.catsDataMonadErrorForKleisli

def fromMaybe[Model, A](a: Action[Model, Option[A]]): Action[Model, A] =
  a.flatMap(_.unwrap)

/** This takes in an event stream and a callback, and provides a binder that
  * will bind the next event thrown by the event stream to be passed into the
  * callback.
  *
  * This needs to return a binder because the subscription to the event stream
  * needs to be managed by an element so that it doesn't leak. Of course, in our
  * case, we will be mostly just binding to the root element which will stay
  * alive throughout the lifetime of the application, but the laminar API
  * doesn't know this, so we have to go through the whole process...
  *
  * This function should almost never be called directly, its main purpose is to
  * be used in `nextEvent`
  */
def bind[T, El <: ReactiveElement.Base](
    es: EventStream[T],
    cb: Either[Throwable, T] => Unit
): Binder[El] = {
  var sub: Option[Subscription] = None
  (element: El) =>
    ReactiveElement.bindSubscription(element) { ctx =>
      val s = es.recoverToTry.foreach { e =>
        import scala.util.{Failure, Success}
        e match {
          case Success(evt)   => cb(Right(evt))
          case Failure(error) => cb(Left(error))
        }
        sub.foreach(_.kill())
        sub = None
      }(ctx.owner)
      sub = Some(s)
      s
    }
}

def amendElt[Model](m: Modifier[SvgElement]): Action[Model, Unit] = for {
  elt <- ops[Model].ask.map(_.elt)
  _ <- ops.delay(elt.amend(m))
} yield ()

/** This is the workhorse connecting Laminar and cats-effect. Given an event
  * stream this function uses asynchronous IO to return the next event in the
  * stream
  *
  * This will essentially pause the current thread of execution until the stream
  * provides the next value, so it makes it look like a synchronous "wait" call,
  * but crucially it does *not* block the main thread, so other stuff can go on
  * while the "local thread" is waiting for the next event.
  */
def nextEvent[Model, A](stream: EventStream[A]): Action[Model, A] = for {
  elt <- ops[Model].ask.map(_.elt)
  evt <- ops.async_[A](cb => elt.amend(bind(stream, cb)))
} yield evt

def nextKeydown[Model]: Action[Model, KeyboardEvent] = for {
  keyboard <- ops[Model].ask.map(_.keyboard)
  evt <- nextEvent(keyboard.keydowns.events)
} yield evt

def nextKey[Model]: Action[Model, String] = nextKeydown.map(_.key)

def nextKeydownIn[Model](set: Set[String]): Action[Model, KeyboardEvent] = for {
  keyboard <- ops[Model].ask.map(_.keyboard)
  evt <- nextEvent(keyboard.keydowns.events.filter(evt => set(evt.key)))
} yield evt

def updateModel[Model](f: Model => Model): Action[Model, Unit] = for {
  $model <- ops[Model].ask.map(_.$model)
  _ <- ops.delay($model.update(f))
} yield {}

def updateModelS[Model, A](updater: State[Model, A]): Action[Model, A] = for {
  $model <- ops[Model].ask.map(_.$model)
  a <- ops.async_[A] { cb =>
    {
      new Transaction({ _ =>
        val model = $model.now()
        val (newModel, a) = updater.run(model).value
        $model.set(newModel)
        cb(Right(a))
      })
    }
  }
} yield a

def getModel[Model]: Action[Model, Var[Model]] = ReaderT.ask.map(_.$model)

def mousePos[Model]: Action[Model, Complex] =
  ops.ask.map(_.mouse.$state.now().pos)

def mouseDown[Model](b: MouseButton): Action[Model, Option[Entity]] = for {
  mouse <- ops[Model].ask.map(_.mouse)
  ent <- nextEvent(mouse.mouseEvents.events.collect({
    case MouseEvent.MouseDown(pos, `b`) => pos
  }))
} yield ent

def hovered[Model]: Action[Model, Option[Entity]] =
  ops.ask.map(_.hover.$state.now().state)

def hoveredEntity[Model](x: EntityType): Action[Model, Option[Entity]] =
  hovered.map(_.flatMap(_.withType(x)))

def getClick[Model](x: EntityType): Action[Model, Entity] = for {
  ent <- fromMaybe(mouseDown(MouseButton.Left))
  i <- ent.withType(x).unwrap(actionMonadError[Model])
} yield i

def update[Model]: Action[Model, Unit] = for {
  updateFun <- ops[Model].ask.map(_.update)
  _ <- ops.delay(updateFun())
} yield ()

def editText[Model](
    listener: Observer[String],
    init: String
): Action[Model, Unit] =
  for {
    bus <- ops[Model].delay(EventBus[Unit]())
    eltDims <- ops[Model].ask.map(_.dims())
    input <- ops.delay(
      TextInput(listener, init, bus.writer, Complex(200, 40), eltDims)
    )
    _ <- addControl(input)
    _ <- nextEvent(bus.events)
    _ <- removeControl(input)
    elt <- ops[Model].ask.map(_.elt)
    _ <- ops.delay(elt.ref.asInstanceOf[js.Dynamic].focus())
  } yield ()

def delay[Model](seconds: Double): Action[Model, Unit] =
  ops.async_(cb => dom.window.setTimeout(() => cb(Right(())), seconds * 1000))

def runUntil[Model](
    action: Action[Model, Unit],
    condition: Action[Model, Unit]
): Action[Model, Unit] = for {
  target <- action.start
  _ <- condition
  _ <- target.cancel
} yield {}

def addRelative[Model](child: SvgElement): Action[Model, Unit] = for {
  childCommands <- ops[Model].ask.map(_.relativeChildCommands)
  _ <- ops.delay(childCommands.onNext(CollectionCommand.Append(child)))
} yield ()

def addControl[Model](child: SvgElement): Action[Model, Unit] = for {
  childCommands <- ops[Model].ask.map(_.controlChildCommands)
  _ <- ops.delay(childCommands.onNext(CollectionCommand.Append(child)))
} yield ()

def removeControl[Model](child: SvgElement): Action[Model, Unit] = for {
  childCommands <- ops[Model].ask.map(_.controlChildCommands)
  _ <- ops.delay(childCommands.onNext(CollectionCommand.Remove(child)))
} yield ()

def zoomBy[Model](factor: Double): Action[Model, Unit] = for {
  transform <- ops[Model].ask.map(_.transform)
  _ <- ops.delay(transform.$state.update(_.zoomBy(factor)))
} yield ()

def zoomAtMouse[Model](factor: Double): Action[Model, Unit] = for {
  transform <- ops[Model].ask.map(_.transform)
  pos <- mousePos
  _ <- ops.delay(transform.$state.update(_.zoomAtPos(factor, pos)))
} yield ()

def dragPan[Model]: Action[Model, Unit] = for {
  drag <- ops[Model].ask.map(_.drag)
  transform <- ops[Model].ask.map(_.transform)
  pos <- mousePos.map(transform.logicalToScreen)
  t <- ops.delay(transform.$state.now())
  _ <- drag.dragStart(Observer(p => {
    transform.$state.set(t.shift(transform.logicalToScreen(p) - pos))
  }))
} yield ()