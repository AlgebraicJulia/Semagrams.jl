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

class EditorState(val elt: SvgElement) {
  val events = EventBus[Event]()
  val viewports = Var(Set[Viewport]())
  val mouse = MouseController()
  val hover = HoverController()
  val drag = DragController()
  val keyboard = KeyboardController()
  val controllers = Seq(mouse, hover, drag, keyboard)
  val size = Var(Complex(elt.ref.clientWidth, elt.ref.clientHeight))

  val entities = Var(EntityCollection())

  val currentView: Var[Part] = Var(ROOT)


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

  def makeViewport[A](state: Signal[A], sources: Seq[EntitySource[A]]) = 
    println("begin mv")
    for {
    v <- IO(new EntitySourceViewport(state, sources))
    _ = println("created esv")
    _ <- IO(register(v))
    _ = println("registered")
  } yield {
    println("end mv")
    v
  }

  def makeUI() = for {
    ui <- IO(new UIState(Var(Vector()), () => (), size.signal))
    _ <- IO(register(ui.viewport))
  } yield ui

  def register(v: Viewport) = {
    println("begin register")
    viewports.update(_ + v)
    println("finished update")
    elt.amend(
      viewports.now().toSeq(0).entities --> entities.writer,
    )
    println("finished amend")
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
            case Success(evt)   => 
              // if evt.isInstanceOf[DoubleClick]
              // then 
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

  def nextEvent[A](stream: EventStream[A]): IO[A] =
    IO.async_(cb => 
      elt.amend(attachEventStream(stream, cb))
    )

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


  
  def bindHelper[A](ev:Event) = ((bnd:Binding[A]) => 
    bnd.modifiers match
      case Some(mods) =>
        if (keyboard.keyState.now().modifiers == mods)
        then bnd.selector.lift(ev)
        else None
      case None => bnd.selector.lift(ev)
  ).unlift
    
  
  def bindNoCatchFirst[A](bindings: Seq[Binding[A]]): IO[A] =
    nextEvent(
      events.events.collect(
        ((ev: Event) =>
          val b = bindings.collectFirst(bindHelper(ev))

          val bs = bindings.collect(bindHelper(ev))

          // ev match
          //   case e @ (_:DoubleClick | _:MouseDown) => println(s"bncf: $b, $bs")
          //   case _ => ()
          
          bs match
            case Seq() => None
            case Seq(b,rest@_*) => Some(doAll(b,rest))
          

        ).unlift
      )
    ).flatten

  def doAll[A](a1: IO[A],as:Seq[IO[A]]): IO[A] = as match
    case Seq() => a1
    case Seq(a2,rest @_*) => for { 
      first <- a1
      // _ = println(first)
      last <- doAll(a2,rest)
    } yield last


  def bind[A](bindings: Seq[Binding[A]]): IO[Option[A]] =
    bindNoCatchFirst[A](bindings).map(Some(_)).handleError(_ => None)

  def bindUntilFail[A](bindings: Seq[Binding[A]]): IO[Unit] =
    bindNoCatchFirst[A](bindings).foreverM.handleError(_ => ())

  def bindForever[A](bindings: Seq[Binding[A]]): IO[Nothing] =
    bind[A](bindings).foreverM

  def mousePos: IO[Complex] =
    IO(mouse.$state.now().pos)


  def bgPart() = currentView.now()
  def bgPlus(p:Part) = bgPart().extend(p)


  def hovered: IO[Option[Entity]] = IO({
    hover.$state.now().state.map(_ match
        case p:Part => bgPlus(p)
        case e => e
    )
  })

  def hoveredPart: IO[Option[Part]] = hovered.map(_ match
      case None => Some(bgPart())
      case Some(p:Part) => Some(p)
      case _ => None
  )

  def hoveredPart(ty: PartType): IO[Option[Part]] =
    hoveredPart.map(_ match
      case Some(p: Part) if p.ty == ty => Some(p)
      case _                           => None
    )

  def hoveredPart(tys: Seq[PartType]): IO[Option[Part]] =
    hoveredPart.map(_ match
      case Some(p: Part) if tys contains p.ty => Some(p)
      case _ => None
    )

  def hoveredEntity(ty: EntityType): IO[Option[Entity]] =
    hovered.map(e =>
      e match {
        case Some(p: Part) if p.ty == ty => Some(p)
        case _                           => None
      }
    )



  import semagrams.widgets.{Menu,PositionWrapper,Position}

  def makeMenu(ui:UIState,entries:Seq[(String,Part => IO[Unit])])(i:Part) = for {
    pos <- mousePos
    choice <- ui.dialogue[Part => IO[Matchable]](
      cb => PositionWrapper(
        Position.atPos(pos), 
        Menu(entries)(cb)
      )
    )
    _ <- choice(i)
  } yield ()



}
