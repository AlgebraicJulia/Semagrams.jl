package semagrams

import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.ui._
import semagrams.util._
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import cats.effect._
import org.scalajs.dom

class EditorState(val elt: SvgElement) {
  val events = EventBus[Event]()
  val viewports = Var(Set[Viewport]())
  val mouse = MouseController()
  val hover = HoverController()
  val drag = DragController()
  val keyboard = KeyboardController()
  val controllers = Seq(mouse, hover, drag, keyboard)
  val size = Var(Complex(elt.ref.clientWidth, elt.ref.clientHeight))

  dom.ResizeObserver(
    (newsize,_) => {
      size.set(Complex(elt.ref.clientWidth, elt.ref.clientHeight))
    }
  ).observe(elt.ref)

  elt.amend(
    children <-- viewports.signal.map(_.map(_.elt).toSeq),
  )

  for (c <- controllers) {
    c(this, elt)
  }

  def makeViewport[A](state: Signal[A], sources: Seq[EntitySource[A]]) = for {
    v <- IO(new EntitySourceViewport(state, sources))
    _ <- IO(register(v))
  } yield v

  def makeUI() = for {
    ui <- IO(new UIState(Var(Vector()), size.signal))
    _ <- IO(register(ui.viewport))
  } yield ui

  def register(v: Viewport) = {
    viewports.update(_ + v)
  }

  def deregister(v: Viewport) = {
    viewports.update(_ - v)
  }

  def dimensions: Complex = Complex(elt.ref.clientWidth, elt.ref.clientHeight)

  def attachEventStream[T](
      es: EventStream[T],
      cb: Either[Throwable, T] => Unit
  ): Binder[SvgElement] = {
    var sub: Option[Subscription] = None
    (element: SvgElement) =>
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

  def nextEvent[A](stream: EventStream[A]): IO[A] =
    IO.async_(cb => elt.amend(attachEventStream(stream, cb)))

  def bindNoCatch[A](bindings: Seq[Binding[A]]): IO[A] =
    nextEvent(events.events.collect(((ev: Event) => {
      bindings.collectFirst(
        (
            (bnd: Binding[A]) =>
              bnd.modifiers match {
                case Some(mods) =>
                  if (keyboard.keyState.now().modifiers == mods) {
                    bnd.selector.lift(ev)
                  } else {
                    None
                  }
                case None => bnd.selector.lift(ev)
              }
        ).unlift
      )
    }).unlift)).flatten

  def bind[A](bindings: Seq[Binding[A]]): IO[Option[A]] =
    bindNoCatch[A](bindings).map(Some(_)).handleError(_ => None)

  def bindUntilFail[A](bindings: Seq[Binding[A]]): IO[Unit] =
    bindNoCatch[A](bindings).foreverM.handleError(_ => ())

  def bindForever[A](bindings: Seq[Binding[A]]): IO[Nothing] =
    bind[A](bindings).foreverM

  def mousePos: IO[Complex] =
    IO(mouse.$state.now().pos)

  def hovered: IO[Option[Entity]] =
    IO(hover.$state.now().state)

  def hoveredPart(ty: PartType): IO[Option[Part]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if p.ty == ty => Some(p)
        case _                           => None
      }
    )

  def hoveredPart(tys: Seq[PartType]): IO[Option[Part]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if tys contains p.ty => Some(p)
        case _                           => None
      }
    )

  def hoveredEntity(ty: EntityType): IO[Option[Entity]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if p.ty == ty => Some(p)
        case _                           => None
      }
    )
}
