package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec
import org.scalajs.dom
import cats.effect._
import cats.effect.std._

import semagrams.acsets._
import semagrams.listeners._
import semagrams.sprites._
import semagrams.util._
import semagrams.bindings.Binding
import semagrams.bindings.Action

trait Semagram {
  type Model

  def svgDefs(): Seq[SvgElement] =
    Seq(
      svg.marker(
        svg.idAttr := "arrowhead",
        svg.markerWidth := "10",
        svg.markerHeight := "7",
        svg.refX := "10",
        svg.refY := "3.5",
        svg.orient := "auto",
        svg.polygon(
          svg.points := "0 0, 10 3.5, 0 7"
        )
      )
    )

  /** Computes dynamically some layout properties.
    *
    * Note that the type signature of this assumes that the layout can be stored
    * in the same type as the input. This is a priori not the case, but is the
    * case for the current way we do layout with ACSets. For the sake of
    * simplicity I'm not going to make two types that are going to be always
    * instantiated to the same type for the forseeable future, but I'm making
    * this note so that we know what to change if we need to do so.
    */
  def layout(m: Model): Model

  /** Extractors for the various entities in the Semagram
    */
  val entitySources: Seq[EntitySource[Model]]

  /** Produces a list of sprites to be rendered.
    *
    * This is a pure function, so it can be tested.
    *
    * Contract: a given entity should only ever be associated with a single
    * sprite. If you emit the same entity with a different sprite, it will not
    * change to a different sprite. However, entities can be associated with
    * multiple ACSets; this will automatically update the sprite associated with
    * the entity.
    */
  def produceSprites(
      m: Model,
      eventWriter: Observer[Event]
  ): Seq[(Entity, ACSet, Sprite)] = EntityCollector.collect(m, entitySources)

  /** Creates the svg element ready to be inserted into a larger app. */
  def apply(mSig: Signal[Model], eventWriter: Observer[Event]): SvgElement = {
    svg.svg(
      svgDefs(),
      svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
      children <-- mSig
        .map(layout)
        .map(produceSprites(_, eventWriter))
        .split(_._1)((ent, init, updates) => {
          val (_, initAcset, sprite) = init
          sprite.present(ent, initAcset, updates.map(_._2), eventWriter)
        }),
      mouseMoveListener(eventWriter),
      MouseEvents.clickHandlers(Background(), eventWriter)
    )
  }

}



type Readout[Model] = () => Model
type Update[Model] = Message[Model] => Unit

case class SemagramElt[Model](
  elt: Element,
  readout: Readout[Model],
  update: Update[Model]
)

trait ACSemagram extends Semagram {
  type Model = ACSet

  val schema: Schema

  def apply(bindings: Seq[Binding[ACSet]],a:ACSet = ACSet(schema)): SemagramElt[Model] = 
    if a.schema != schema
    then SemagramElt(
      div(s"ACSet schema ${a.schema} != Semagram schema $schema"),
      () => ACSet(schema),
      _ => ACSet(schema)
    )
    else {
      val modelVar = UndoableVar(a)
      val stateVar = Var(EditorState(None, Complex(0,0)))
      val globalStateVar = Var(GlobalState(Set()))
      val eventBus = EventBus[Event]()
      val globalEventBus = EventBus[Event]()
      val modelSig = EditorState.modifyACSet(modelVar.signal, stateVar.signal)
      val outbox = EventBus[Message[Model]]()


      val semaElt = div(
        cls := "semaElt",
        this.apply(modelSig,eventBus.writer).amend(
          svg.height := "100%",
          svg.width := "100%",
        ),
        globalEventBus.events --> globalStateVar.updater[Event]((globalState, evt) => globalState.processEvent(evt)),
        globalEventBus.events --> eventBus.writer,
        backgroundColor := "blue",
      )

      // It works here
      semaElt.amend(
        backgroundColor := "white"
      )

      GlobalState.listen(globalEventBus.writer)

      val main = for {
        eventQueue <- Queue.unbounded[IO, Event]
        _ <- Dispatcher.sequential[IO] use { dispatcher =>
          semaElt.amend(
            eventBus.events --> Observer[Event](evt =>
              dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
            )
          )
          Binding.processAll(Action.Resources(modelVar, stateVar, globalStateVar.signal, eventQueue, outbox.writer), bindings)
        }
      } yield ()

      main.unsafeRunAndForget()(unsafe.IORuntime.global)

      SemagramElt(
        semaElt,
        () => modelVar.now(),
        (msg:Message[ACSet]) => modelVar.update(msg.execute)
      )
    }

}