package semagrams.controllers

import com.raquo.laminar.api.L._
import cats.data._
import cats.effect._
import cats.effect.unsafe.IORuntime
import scala.concurrent.ExecutionContext.Implicits.global
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util._
import semagrams._
import org.scalajs.dom.raw.KeyboardEvent
import com.raquo.laminar.nodes.ReactiveElement
import io.laminext.websocket.WebSocket
import io.laminext.websocket.upickle._
import upickle.default.{ReadWriter => RW, _}


case class EditorState[Model](
  mouse: MouseController,
  drag: DragController,
  hover: HoverController,
  keyboard: KeyboardController,
  $model: Var[Model],
  elt: SvgElement
)

object EditorState {
  def apply[Model]($model: Var[Model], elt: SvgElement) = {
    val mouse = MouseController()
    val drag = DragController(mouse)
    val hover = HoverController()
    val keyboard = KeyboardController()
    // val ws = WebSocket.url(url).json[In,Out].build(managed=true)

    elt.amend(
      mouse,
      drag,
      hover,
      keyboard,
      // ws.connect
    )

    new EditorState(mouse, drag, hover, keyboard, $model, elt)
  }
}

type Action[Model, A] = ReaderT[IO, EditorState[Model], A]

implicit def actionLiftIO[Model]: LiftIO[[A] =>> Action[Model,A]] = LiftIO.catsKleisliLiftIO

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
          case Success(evt) => cb(Right(evt))
          case Failure(error) => cb(Left(error))
        }
        sub.foreach(_.kill())
        sub = None
      }(ctx.owner)
      sub = Some(s)
      s
    }
}

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

def nextKeydownIn[Model](set: Set[String]): Action[Model, KeyboardEvent] = for {
  keyboard <- ReaderT.ask.map(_.keyboard)
  evt <- nextEvent(keyboard.keydowns.events.filter(evt => set(evt.key)))
} yield evt

def runAction[Model, A](
  state: EditorState[Model],
  action: Action[Model, Unit]
): Unit = {
  action.run(state).unsafeRunAndForget()(IORuntime.global)
}

case class KeyBindings[Model](bindings: Map[String, Action[Model, Unit]]) {
  def run: Action[Model, Unit] =
    nextKeydownIn(bindings.keySet).flatMap((evt: KeyboardEvent) => bindings(evt.key))
}

def updateModel[Model](f: Model => Model): Action[Model, Unit] = {
  val L = actionLiftIO[Model]
  for {
    $model <- ReaderT.ask.map(_.$model)
    _ <- L.liftIO(IO($model.update(f)))
  } yield {}
}

def mousePos[Model]: Action[Model, Complex] =
  ReaderT.ask.map(_.mouse.$state.now().pos)

def hovered[Model]: Action[Model, Option[Entity]] =
  ReaderT.ask.map(_.hover.$state.now().state)
