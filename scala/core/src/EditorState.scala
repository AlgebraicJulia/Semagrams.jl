package semagrams

import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.ui._
import semagrams.util._
import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveElement
import cats.effect._
import cats.implicits._
import org.scalajs.dom

/** This is the main state for Semagrams.
  *
  * Even though we call this "state", notice that everything here is actually
  * immutable. The state itself is all stored in Laminar stuff, like Vars
  * and EventBuses.
  */
class EditorState(val elt: SvgElement) {
  /** The main event bus for Semagrams. */
  val events = EventBus[Event]()

  /** The viewports in current use */
  val viewports = Var(Set[Viewport]())

  val mouse = MouseController()
  val hover = HoverController()
  val drag = DragController()
  val keyboard = KeyboardController()

  /** all of the controllers */
  val controllers = Seq(mouse, hover, drag, keyboard)

  /** The current size of the main element. Kept in sync by another line later. */
  val size = Var(Complex(elt.ref.clientWidth, elt.ref.clientHeight))

  /** All the entities in all of the viewports, and their associated sprites. */
  val entities = Var(EntityCollection())

  dom
    .ResizeObserver((newsize, _) => {
      size.set(Complex(elt.ref.clientWidth, elt.ref.clientHeight))
    })
    .observe(elt.ref)

  // Attach all of the root elements of the viewports to the main element
  elt.amend(
    children <-- viewports.signal.map(_.map(_.elt).toSeq)
  )

  for (c <- controllers) {
    c(this, elt)
  }

  /** Construct a new viewport
    *
    * @param state
    *   the viewport is a view on this signal
    *
    * @param sources
    *   the viewport has entities extracted using these [[EntitySource]]s
    */
  def makeViewport[A](state: Signal[A], sources: Seq[EntitySource[A]]) = for {
    v <- IO(new EntitySourceViewport(state, sources))
    _ <- IO(register(v))
  } yield v

  /** Make a new [[UIState]] object and register its viewport */
  def makeUI() = for {
    ui <- IO(new UIState(Var(Vector()), () => (), size.signal))
    _ <- IO(register(ui.viewport))
  } yield ui

  /** Register a viewport.
    * 
    * This adds the viewport to [[viewports]], which has the side-effect of
    * attaching its main element to `this.elt` while the viewport is still in
    * [[viewports]]
    */
  def register(v: Viewport) = {
    viewports.update(_ + v)
    elt.amend(
      viewports.now().toSeq(0).entities --> entities.writer
    )
  }

  /** Deregister a viewport, which has the side effect of removing its main
    * element from [[elt]]
    */
  def deregister(v: Viewport) = {
    viewports.update(_ - v)
  }

  /** Deprecated in favor of [[size]] */
  def dimensions: Complex = Complex(elt.ref.clientWidth, elt.ref.clientHeight)

  /** Attach a callback to `es` so that the callback will be called with the
    * next element to come out. This returns a binder that needs to be attached
    * to an element to manage the subscription that is created.
    *
    * Note: we are going to move away from this soon.
    *
    * @param es
    *   The event stream to attach to
    *
    * @param cb
    *   The callback to be called
    */
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
            case Success(evt) =>
              cb(Right(evt))
            case Failure(error) =>
              cb(Left(error))
          }
          sub.foreach(_.kill())
          sub = None
        }(ctx.owner)
        sub = Some(s)
        s
      }
  }

  /** Return the next event to come out of `stream` asynchronously */
  def nextEvent[A](stream: EventStream[A]): IO[A] =
    IO.async_(cb => elt.amend(attachEventStream(stream, cb)))

  /** Get the next event that matches one of the bindings, and then execute the
    * action that is associated to it.
    */
  def bindNoCatch[A](bindings: Seq[Binding[A]]): IO[A] =
    nextEvent(events.events.collect(((ev: Event) => {
      bindings.collectFirst(
        (
            (bnd: Binding[A]) =>
              bnd.modifiers match {
                case Some(mods) => {
                  if (keyboard.keyState.now().modifiers == mods) {
                    bnd.selector.lift(ev)
                  } else {
                    None
                  }
                }
                case None => bnd.selector.lift(ev)
              }
        ).unlift
      )
    }).unlift)).flatten

  def bindHelper[A](ev: Event) = (
      (bnd: Binding[A]) =>
        bnd.modifiers match
          case Some(mods) =>
            if (keyboard.keyState.now().modifiers == mods)
            then bnd.selector.lift(ev)
            else None
          case None => bnd.selector.lift(ev)
  ).unlift

  /** Same thing as [[bindNoCatch]] except when an event matches some binding,
    * it runs *all* of the bindings that it matches, not just the first one.
    */
  def bindNoCatchFirst[A](bindings: Seq[Binding[A]]): IO[A] =
    nextEvent(
      events.events.collect(
        ((ev: Event) =>
          val b = bindings.collectFirst(bindHelper(ev))

          val bs = bindings.collect(bindHelper(ev))

          bs match
            case Seq()             => None
            case Seq(b, rest @ _*) => Some(doAll(b, rest))
        ).unlift
      )
    ).flatten

  def doAll[A](a1: IO[A], as: Seq[IO[A]]): IO[A] = as match
    case Seq() => a1
    case Seq(a2, rest @ _*) =>
      for {
        first <- a1
        last <- doAll(a2, rest)
      } yield last

  /** Same thing as [[bindNoCatch]], except it handles any errors and returns
    * None when there is one.
    */
  def bind[A](bindings: Seq[Binding[A]]): IO[Option[A]] =
    bindNoCatchFirst[A](bindings).map(Some(_)).handleError(_ => None)

  /** Run the bindings repeatedly until there is an error, and then stop */
  def bindUntilFail[A](bindings: Seq[Binding[A]]): IO[Unit] =
    bindNoCatchFirst[A](bindings).foreverM.handleError(_ => ())

  /** Run the bindings forever, ignoring any errors */
  def bindForever[A](bindings: Seq[Binding[A]]): IO[Nothing] =
    bind[A](bindings).foreverM

  /** An IO action that when run, returns the current mouse position */
  def mousePos: IO[Complex] =
    IO(mouse.$state.now().pos)

  /** An IO action that when run, returns the current hovered entity */
  def hovered: IO[Option[Entity]] =
    IO(hover.$state.now().state)

  /** An IO action that filters [[hovered]] for just [[Part]]s
    */
  def hoveredPart: IO[Option[Part]] = hovered.map(h =>
    h match
      case Some(e) =>
        e match
          case Background() => Some(ROOT)
          case p: Part      => Some(p)
          case _            => None
      case None => Some(ROOT)
  )

  /** An IO action that filters [[hovered]] for just [[Part]]s of a certain
    * type.
    */
  def hoveredPart(ty: PartType): IO[Option[Part]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if p.ty == ty => Some(p)
        case _                           => None
      }
    )

  /** An IO action that filters [[hovered]] for just [[Part]]s that are one
    * of several types.
    */
  def hoveredPart(tys: Seq[PartType]): IO[Option[Part]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if tys contains p.ty => Some(p)
        case _                                  => None
      }
    )

  /** An IO action that filters [[hovered]] for entities of a certain type.
    */
  def hoveredEntity(ty: EntityType): IO[Option[Entity]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if p.ty == ty => Some(p)
        case _                           => None
      }
    )

  import semagrams.widgets.{Menu, PositionWrapper, Position}

  /** Constructs a menu at the current mouse position */
  def makeMenu(ui: UIState, entries: Seq[(String, Part => IO[Unit])])(i: Part) =
    for {
      pos <- mousePos
      choice <- ui.dialogue[Part => IO[Matchable]](cb =>
        PositionWrapper(
          Position.atPos(pos),
          Menu(entries)(cb)
        )
      )
      _ <- choice(i)
    } yield ()
}
