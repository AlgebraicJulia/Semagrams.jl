package semagrams.actions

import com.raquo.laminar.api.L._
import cats.Monad
import cats.data._
import semagrams.controllers._
import semagrams.acsets._
import cats.effect._
import cats.effect.unsafe.IORuntime
import scala.concurrent.ExecutionContext.Implicits.global
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util._
import semagrams._
import org.scalajs.dom.raw.KeyboardEvent
import com.raquo.laminar.nodes.ReactiveElement
import monocle.std.these
import cats.instances.stream
import upickle.default._

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

    elt.amend(
      mouse,
      drag,
      hover,
      keyboard
    )

    new EditorState(mouse, drag, hover, keyboard, $model, elt, update)
  }
}

type Action[Model, A] = ReaderT[IO, EditorState[Model], A]

implicit def actionLiftIO[Model]: LiftIO[[A] =>> Action[Model, A]] =
  LiftIO.catsKleisliLiftIO

def runAction[Model](
    state: EditorState[Model],
    action: Action[Model, Unit]
): Unit = {
  action.run(state).unsafeRunAndForget()(IORuntime.global)
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

case class KeyBindings[Model](bindings: Map[String, Action[Model, Unit]]) {

  /** This listens for a keybinding, and then executes the action associated
    * with that keybinding. Crucially, this only runs *once*. If you want to run
    * this in a loop, you should provide that loop yourself.
    */
  def run: Action[Model, Unit] =
    nextKeydownIn(bindings.keySet).flatMap((evt: KeyboardEvent) =>
      bindings(evt.key)
    )

  def runForever: Action[Model, Unit] = {
    val T = implicitly[Monad[[X] =>> Action[Model, X]]]
    T.foreverM(run)
  }
}

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
    model <- L.liftIO(IO($model.now()))
    (newModel, a) = updater.run(model).value
    _ <- L.liftIO(IO($model.set(newModel)))
  } yield a
}

def addChild[Model](child: SvgElement): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    elt <- ReaderT.ask.map(_.elt)
    _ <- L.liftIO(IO(elt.amend(child)))
  } yield ()
}

def mousePos[Model]: Action[Model, Complex] =
  ReaderT.ask.map(_.mouse.$state.now().pos)

def mouseDown[Model](b: MouseButton): Action[Model, Complex] = for {
  mouse <- ReaderT.ask.map(_.mouse)
  pos <- nextEvent(mouse.mouseEvents.events.collect({
    case MouseEvent.MouseDown(pos, `b`) => pos
  }))
} yield pos

def hovered[Model]: Action[Model, Option[Entity]] =
  ReaderT.ask.map(_.hover.$state.now().state)

def hoveredPart[Model, X <: Ob](x: X): Action[Model, Option[Elt[X]]] =
  hovered.map(_.flatMap(_.asElt(x)))

def update[Model]: Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    updateFun <- ReaderT.ask.map(_.update)
    _ <- L.liftIO(IO(updateFun()))
  } yield ()
}
