package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

import cats.effect._
import cats.effect.std._

import semagrams.acsets._
import semagrams.listeners._
import semagrams.sprites._
import semagrams.util._
import semagrams.bindings.Binding
import semagrams.bindings.Action
import semagrams.widgets.PropTable


/** A trait for generic semagrams with arbitrary `Model` types **/
trait Semagram {
  type Model

  /* Default svg element properties */
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
      MouseEvents.handlers(Background(), eventWriter)
    )
  }

}



/** A class packaging the laminar element of a semagram along with
 *  the schema and accessors/modifiers
 */
case class SemagramElt(
  elt: Div,
  schema: Schema,
  readout: () => ACSet,
  signal: Signal[ACSet],
  messenger: Observer[Message[ACSet]]
):

  /** A callback function for passing messages externally **/
  def update(msg:Message[ACSet]) = messenger.onNext(msg)

  /** Convenience method for constructing tables from semagrams **/
  def propTable(ob:Ob,cols:Seq[Property],keys:Seq[Property]) = 
    PropTable(ob,cols,keys)

  def propTable(ob:Ob,cols:Seq[Property],key:Property): PropTable =
    propTable(ob,cols,Seq(key))
  
  def propTable(ob:Ob,cols:Seq[Property]): PropTable =
    propTable(ob,cols,Seq())

  def propTable(ob:Ob): PropTable =
    val cols = schema.homs.filter(_.doms.contains(PartType(Seq(ob))))
      ++ schema.attrs.filter(_.doms.contains(PartType(Seq(ob))))
    propTable(ob,cols) 


  /** Convenience method for constructing a laminar table element
   *  directly from a Semagram
   */
  def laminarTable(ob:Ob,cols:Seq[Property],keys:Seq[Property] = Seq()) =
    propTable(ob,cols,keys).laminarElt(signal,messenger)

  def laminarTable(ob:Ob) = propTable(ob)

/** A specialized trait for semagrams with Model == ACSet **/
trait ACSemagram extends Semagram {
  type Model = ACSet

  val schema: Schema

  /** Construct a `SemagramElt` from a collection of `Binding` interactions
   *  and an optional initial value
   */ 
  def apply(bindings: Seq[Binding[ACSet]],a:ACSet = ACSet(schema)): SemagramElt = {
    
    /* Construct the ACSet variable */
    val modelVar = if a.schema == schema 
      then UndoableVar(a) 
      else 
        println(s"Bad input schema ${a.schema} != $schema")
        UndoableVar(ACSet(schema))

    /* Construct state variable (attached to the element) and global
     *  state variable (attached to the window) */
    val stateVar = Var(EditorState(Some(Background()), Complex(0,0),Complex(1,1)))
    val globalStateVar = Var(GlobalState(Set()))
        
    /* Construct event buses to manipulate state variables */
    val eventBus = EventBus[Event]()
    val globalEventBus = EventBus[Event]()
    
    /* Modify model to account for local state (e.g., hover) */ 
    val modelSig = EditorState.modifyACSet(modelVar.signal, stateVar.signal)

    // I built messages into the `Event` structure (`MsgEvent`) before I 
    // really understood the laminar API with a `MsgEvent`.
    // Refactor to use it or remove?
    val outbox = EventBus[Message[Model]]()

    val defaultAttrs = Seq(
      backgroundColor := "lightblue",
      height := "400px",
      width := "100%",
      border := "black",
      borderStyle := "solid",
      boxSizing := "border-box",
    )
    
    
    /* Construct the laminar element associated with the semagram */
    val semaElt = div(
      this.apply(modelSig,eventBus.writer).amend(
        svg.height := "100%",
        svg.width := "100%",
      ),
      globalEventBus.events --> globalStateVar.updater[Event]((globalState, evt) => globalState.processEvent(evt)),
      globalEventBus.events --> eventBus.writer,
      defaultAttrs
    )
    

    /* Attach global (keyboard) listeners */
    GlobalState.listen(globalEventBus.writer)

    /* Start cats.io event queue */
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

    /* Return associated `SemagramElt` */
    SemagramElt(
      semaElt,
      schema,
      () => modelVar.now(),
      modelSig,
      modelVar.writer.contramap(
        msg => msg.execute(modelVar.now())
      )
    )
  }


}