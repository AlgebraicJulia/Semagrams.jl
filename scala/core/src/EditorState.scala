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
import cats.effect.std._

/** This is the main state for Semagrams.
  *
  * Even though we call this "state", notice that everything here is actually
  * immutable. The state itself is all stored in Laminar stuff, like Vars and
  * EventBuses.
  *
  * We pass in `dispatcher` and `eventQueue` because this constructor can't run
  * in IO, and thus can't make them itself. It can however use the `dispatcher`
  * to attach the EventBus of events to the queue.
  *
  * @param elt
  *   The main svg element of the semagram.
  *
  * @param dispatcher
  *   An IO dispatcher, used to bind the EventBus of events to `eventQueue`.
  *
  * @param eventQueue
  *   A queue to use for events. We might also throw things into this queue for
  *   testing purposes.
  */
class EditorState(
    val elt: SvgElement,
    dispatcher: Dispatcher[IO],
    val eventQueue: Queue[IO, Event]
) {

  /** The main event bus for Semagrams. */
  val events = EventBus[Event]()

  /** The viewports in current use */
  val viewports = Var(Map[String, Viewport]())

  val mouse = MouseController()
  val hover = HoverController()
  val drag = DragController()
  val keyboard = KeyboardController()

  /** all of the controllers */
  val controllers = Seq(mouse, hover, drag, keyboard)

  /** The current size of the main element. Kept in sync by another line later.
    */
  val size = Var(Complex(elt.ref.clientWidth, elt.ref.clientHeight))

  /** All the entities in all of the viewports, and their associated sprites. */
  val entities = Var(EntityCollection())

  /** The current part shown in the viewport. Used for nested diagrams
    * (zooming).
    */
  val currentView: Var[Part] = Var(ROOT)

  dom
    .ResizeObserver((newsize, _) =>
      size.set(Complex(elt.ref.clientWidth, elt.ref.clientHeight))
    )
    .observe(elt.ref)

  // Attach all of the root elements of the viewports to the main element
  elt.amend(
    children <-- viewports.signal.map(_.map(_._2.elt).toSeq),
    events --> Observer[Event](evt =>
      dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
    )
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
  def makeViewport[A](
      vpname: String,
      state: Signal[A],
      sources: Seq[EntitySource[A]]
  ): IO[EntitySourceViewport[A]] = for {
    v <- IO(new EntitySourceViewport(state, sources))
    _ <- IO(register(vpname, v))
  } yield v

  /** Make a new [[UIState]] object and register its viewport */
  def makeUI(): IO[UIState] = for {
    ui <- IO(new UIState(Var(Vector()), () => (), size.signal))
    _ <- IO(register("uiVP", ui.viewport))
  } yield ui

  /** Register a viewport.
    *
    * This adds the viewport to [[viewports]], which has the side-effect of
    * attaching its main element to `this.elt` while the viewport is still in
    * [[viewports]]
    */
  def register(vpname: String, v: Viewport) = {
    viewports.update(_ + (vpname -> v))
    elt.amend(
      viewports.now()("mainVP").entities --> entities.writer
    )
  }

  /** Deregister a viewport, which has the side effect of removing its main
    * element from [[elt]]
    */
  def deregister(vname: String) = {
    viewports.update(_.removed(vname))
  }

  /** Deprecated in favor of [[size]] */
  def dimensions: Complex = Complex(elt.ref.clientWidth, elt.ref.clientHeight)

  /** Get the next event that matches one of the bindings, and then execute the
    * action that is associated to it.
    */
  def bindNoCatch[A](bindings: Seq[Binding[A]]): IO[A] =
    for {
      evt <- eventQueue.take
      actionOption = bindings.collectFirst(
        (
            (bnd: Binding[A]) =>
              bnd.modifiers match {
                case Some(mods) => {
                  if (keyboard.keyState.now().modifiers == mods) {
                    bnd.selector.lift(evt)
                  } else {
                    None
                  }
                }
                case None => bnd.selector.lift(evt)
              }
        ).unlift
      )
      a <- actionOption match {
        case Some(action) => action
        case None         => bindNoCatch(bindings)
      }
    } yield a

  /** Same thing as [[bindNoCatch]], except it handles any errors and returns
    * None when there is one.
    */
  def bind[A](bindings: Seq[Binding[A]]): IO[Option[A]] =
    bindNoCatch[A](bindings).map(Some(_)).handleError(_ => None)

  /** Run the bindings repeatedly until there is an error, and then stop */
  def bindUntilFail[A](bindings: Seq[Binding[A]]): IO[Unit] =
    bindNoCatch[A](bindings).foreverM.handleError(_ => ())

  /** Run the bindings forever, ignoring any errors */
  def bindForever[A](bindings: Seq[Binding[A]]): IO[Nothing] =
    bind[A](bindings).foreverM

  /** An IO action that when run, returns the current mouse position */
  def mousePos: IO[Complex] =
    IO(mouse.$state.now().pos)

  /** Get the current background part */
  def bgPart: Part = currentView.now()

  /** Extend the current background part by `p` */
  def bgPlus(p: Part): Part = bgPart.extendPart(p)

  /** An IO action that when run, returns the current hovered entity */
  def hovered: IO[Option[Entity]] = IO({
    hover.$state.now().state.map(_ match
      case p: Part => bgPlus(p)
      case e       => e
    )
  })

  /** An IO action that filters [[hovered]] for just [[Part]]s and converts
    * `None` the background part (e.g., `ROOT`)
    */
  def hoveredPart: IO[Option[Part]] = hovered.map(_ match
    case Some(p: Part) => Some(p)
    case Some(e)       => None
    case None          => Some(bgPart)
  )

  /** An IO action that filters [[hovered]] for just [[Part]]s of a certain
    * type.
    */
  def hoveredPart(ty: PartType): IO[Option[Part]] =
    hoveredPart.map(_ match
      case Some(p: Part) if p.ty == ty => Some(p)
      case _                           => None
    )

  /** An IO action that filters [[hovered]] for just [[Part]]s that are one of
    * several types.
    */
  def hoveredPart(tys: Seq[PartType]): IO[Option[Part]] =
    hoveredPart.map(_ match
      case Some(p: Part) if tys contains p.ty => Some(p)
      case _                                  => None
    )

  /** An IO action that filters [[hovered]] for entities of a certain type.
    */
  def hoveredEntity(ty: EntityType): IO[Option[Entity]] =
    hovered.map(_ match
      case Some(e) if e.ty == ty => Some(e)
      case _                     => None
    )

  import semagrams.widgets.{Menu, PositionWrapper, Position}

  /** Constructs a menu at the current mouse position indexed by part `i` */
  def makeMenu(ui: UIState, entries: Seq[(String, Part => IO[Unit])])(i: Part) =
    for {
      pos <- mousePos
      choice <- fromMaybe(
        ui.dialogue[Option[Part => IO[Matchable]]](cb =>
          PositionWrapper(
            Position.atPos(pos),
            Menu(entries)(cb)
          )
        )
      )
      _ <- choice(i)
    } yield ()

}
