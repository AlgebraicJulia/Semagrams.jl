package semagrams.actions

import cats.ApplicativeError
import cats.Monad
import cats.MonadError
import cats.data._
import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.instances.stream
import com.raquo.airstream.core.Transaction
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.laminar.nodes.ReactiveSvgElement
import org.scalajs.dom.raw.KeyboardEvent
import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.util._
import upickle.default._
import org.scalajs.dom

import scala.concurrent.ExecutionContext.Implicits.global

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

case class EditorState[Model](
    mouse: MouseController,
    drag: DragController,
    hover: HoverController,
    keyboard: KeyboardController,
    text: TextController,
    tip: TipController,
    bindables: EventBus[Any],
    $model: Var[Model],
    elt: SvgElement,
    update: () => Unit
)

object EditorState {
  def apply[Model]($model: Var[Model], elt: SvgElement, update: () => Unit) = {
    val mouse = MouseController()
    val drag = DragController(mouse)
    val hover = HoverController()
    val keyboard = KeyboardController()
    val text = TextController()
    val bindables = EventBus[Any]()
    val tip = TipController()

    elt.amend(
      mouse,
      drag,
      hover,
      keyboard,
      text,
      tip,
      mouse.mouseEvents --> bindables,
      keyboard.keydowns --> bindables,
      keyboard.keyups --> bindables
    )

    new EditorState(
      mouse,
      drag,
      hover,
      keyboard,
      text,
      tip,
      bindables,
      $model,
      elt,
      update
    )
  }
}

type Action[Model, A] = ReaderT[IO, EditorState[Model], A]

extension [Model, A](action: Action[Model, A]) {
  def bracket[B](
      f: A => Action[Model, B]
  )(g: A => Action[Model, Unit]): Action[Model, B] = {
    Action(es => action(es).bracket(a => f(a)(es))(a => g(a)(es)))
  }

  def toOption: Action[Model, Option[A]] = {
    Action(es => action(es).map(Some(_)).handleError(_ => None))
  }

  def start: Action[Model, Fiber[IO, Throwable, A]] =
    Action[Model, Fiber[IO, Throwable, A]](es => action(es).start)

  def forever: Action[Model, Unit] = {
    val T = implicitly[Monad[Action[Model, _]]]
    T.foreverM(action)
  }

  def onCancel(fin: Action[Model, Unit]) =
    Action[Model, A](es => action(es).onCancel(fin(es)))

  def onCancelOrError(fin: Action[Model, A]) =
    Action[Model, A](es =>
      action(es).onCancel(fin(es).map(_ => ())).handleErrorWith(_ => fin(es))
    )
}

object Action {
  def pure[Model, A](a: A) = Kleisli.pure[IO, EditorState[Model], A](a)

  def apply[Model, A](f: EditorState[Model] => IO[A]): Action[Model, A] =
    Kleisli[IO, EditorState[Model], A](f)
}

implicit def actionLiftIO[Model]: LiftIO[Action[Model, _]] =
  LiftIO.catsKleisliLiftIO

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

def fromMaybe[Model, A](a: Action[Model, Option[A]]): Action[Model, A] = {
  a.flatMap(_.unwrap)
}

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

/** This is the workhorse connecting Laminar and cats-effect. Given an event
  * stream this function uses asynchronous IO to return the next event in the
  * stream
  *
  * This will essentially pause the current thread of execution until the stream
  * provides the next value, so it makes it look like a synchronous "wait" call,
  * but crucially it does *not* block the main thread, so other stuff can go on
  * while the "local thread" is waiting for the next event.
  */
def nextEvent[Model, A](stream: EventStream[A]): Action[Model, A] = {
  val L = actionLiftIO[Model]
  for {
    elt <- ReaderT.ask.map(_.elt)
    evt <- L.liftIO[A](IO.async_(cb => elt.amend(bind(stream, cb))))
  } yield evt
}

def nextKeydown[Model]: Action[Model, KeyboardEvent] = for {
  keyboard <- ReaderT.ask.map(_.keyboard)
  evt <- nextEvent(keyboard.keydowns.events)
} yield evt

def nextKey[Model]: Action[Model, String] = nextKeydown.map(_.key)

def nextKeydownIn[Model](set: Set[String]): Action[Model, KeyboardEvent] = for {
  keyboard <- ReaderT.ask.map(_.keyboard)
  evt <- nextEvent(keyboard.keydowns.events.filter(evt => set(evt.key)))
} yield evt

def updateModel[Model](f: Model => Model): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    $model <- ReaderT.ask.map(_.$model)
    _ <- L.liftIO(IO($model.update(f)))
  } yield {}
}

def updateModelS[Model, A](updater: State[Model, A]): Action[Model, A] = {
  val L = actionLiftIO[Model]
  for {
    $model <- ReaderT.ask.map(_.$model)
    a <- L.liftIO(IO.async_[A] { cb =>
      {
        new Transaction({ _ =>
          val model = $model.now()
          val (newModel, a) = updater.run(model).value
          $model.set(newModel)
          cb(Right(a))
        })
      }
    })
  } yield a
}

def getModel[Model]: Action[Model, Var[Model]] = ReaderT.ask.map(_.$model)

def addChild[Model](child: SvgElement): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    elt <- ReaderT.ask.map(_.elt)
    _ <- L.liftIO(IO(elt.amend(child)))
  } yield ()
}

def mousePos[Model]: Action[Model, Complex] =
  ReaderT.ask.map(_.mouse.$state.now().pos)

def mouseDown[Model](b: MouseButton): Action[Model, Option[Entity]] = for {
  mouse <- ReaderT.ask.map(_.mouse)
  ent <- nextEvent(mouse.mouseEvents.events.collect({
    case MouseEvent.MouseDown(pos, `b`) => pos
  }))
} yield ent

def hovered[Model]: Action[Model, Option[Entity]] =
  ReaderT.ask.map(_.hover.$state.now().state)

def hoveredPart[Model, X <: Ob](x: X): Action[Model, Option[Elt[X]]] =
  hovered.map(_.flatMap(_.asElt(x)))

def getClick[X <: Ob, Model](x: X): Action[Model, Elt[X]] = for {
  ent <- fromMaybe(mouseDown(MouseButton.Left))
  i <- ent.asElt(x).unwrap(actionMonadError[Model])
} yield i

def update[Model]: Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    updateFun <- ReaderT.ask.map(_.update)
    _ <- L.liftIO(IO(updateFun()))
  } yield ()
}

def editText[Model](
    listener: Observer[String],
    init: String
): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    text <- ReaderT.ask.map(_.text)
    _ <- L.liftIO(IO(text.editText(listener, init)))
  } yield ()
}

def editTextBlocking[Model](
    listener: Observer[String],
    init: String
): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    text <- ReaderT.ask.map(_.text)
    _ <- L.liftIO(IO.async_(cb => text.editTextBlocking(listener, init, () => cb(Right(())))))
  } yield ()
}

def showTip[Model](s: String*): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    tip <- ReaderT.ask.map(_.tip)
    _ <- L.liftIO(IO(tip.show(s*)))
  } yield ()
}

def hideTip[Model]: Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    tip <- ReaderT.ask.map(_.tip)
    _ <- L.liftIO(IO(tip.hide()))
  } yield ()
}

def delay[Model](seconds: Double): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  L.liftIO(
    IO.async_(cb => dom.window.setTimeout(() => cb(Right(())), seconds * 1000))
  )
}
