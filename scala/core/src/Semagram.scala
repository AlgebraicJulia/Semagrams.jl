package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

import org.scalajs.dom

import cats.effect._
import cats.effect.std._

// import semagrams.acsets._
import semagrams.listeners._
import semagrams.sprites._
import semagrams.util._
import semagrams.widgets._
import semagrams.bindings.Binding
import semagrams.bindings.Action
import semagrams.widgets.PropTable
import semagrams.acsets.abstr._
import scala.annotation.targetName


/** A trait for generic semagrams with arbitrary `Model` types **/
trait Semagram[D:PartData] {
  type Model

  type Data = D
  // import dataIsPartData._

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
  val entitySources: Seq[EntitySource[Data,Model]]

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
  ): Seq[(Part, Data, Sprite[Data])] = 
    EntityCollector.collect(m, entitySources)
    
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
      MouseEvents.handlers(backgroundPart, eventWriter),
    )
  }

}


/** A class packaging the laminar element of a semagram along with
 *  the schema and accessors/modifiers
 */
case class SemagramElt[D:PartData,A:ACSetWithData[D]](
  elt: Div,
  readout: () => (A,EditorState),
  signal: Signal[(A,EditorState)],
  messageBus: EventBus[Either[Message[A],Message[EditorState]]]
):



  val modelSig = signal.map(_._1)
  val stateSig = signal.map(_._2)

  def getModel() = readout()._1
  def getState() = readout()._2

  def modelEvents: EventStream[Message[A]] = messageBus.events.collect {
    case Left(msg) => msg
  }
  def modelObs: Observer[Message[A]] = messageBus.writer.contramap(
    msg => Left(msg)
  )

  def stateEvents: EventStream[Message[EditorState]] = messageBus.events.collect {
    case Right(msg) => msg
  }
  def stateObs: Observer[Message[EditorState]] = messageBus.writer.contramap {
    case msg => Right(msg)
  }


  case class SemaTable(ob:Ob,cols:Seq[Property],keys:Seq[Property] = Seq()):
    val (editMsgs,editObs) = EventStream.withObserver[(Part,Property)]

    def table = PropTable[Part](ob.label,cols,keys)
    
    val elt = table.laminarElt(
      modelSig.map(_.getPropSeq(ob)),
      editMsgs.map(toEdit => EditMsg(None,Some(toEdit))),
      modelObs.contramap(acsetMsg)
    )

  def acsetMsg(msg:ChangeMsg[Part]): Message[A] = 
    msg match
    case SetValue(p,change) => ChangePropMsg(p,change)
    case HighlightMsg(p,highlighted) => if highlighted 
      then ChangePropMsg(p,PropChange(Highlight,None,()))
      else ChangePropMsg(p,PropChange(Highlight,(),None))

  // /** A callback function for passing messages externally **/
  def update(msg:Message[A]) = modelObs.onNext(msg)
  @targetName("updateState")
  def update(msg:Message[EditorState]) = stateObs.onNext(msg)



  /** Convenience method for constructing tables from semagrams **/
  def propTable(ob:Ob,cols:Seq[Property],keys:Seq[Property]) = 
    SemaTable(ob,cols,keys)

  def propTable(ob:Ob,cols:Seq[Property],key:Property): SemaTable =
    propTable(ob,cols,Seq(key))
  
  def propTable(ob:Ob,cols:Property*): SemaTable =
    propTable(ob,cols,Seq())

  // def propTable(ob:Ob): SemaTable =
  //   val sch = getModel().schema
  //   val cols = sch.homs.filter(_.dom == ob)
  //     ++ sch.attrs.filter(_.dom == ob)
  //   propTable(ob,cols:_*)



/** A specialized trait for semagrams with Model == ACSet **/
trait ACSemagram[D:PartData,A:ACSetWithData[D]] extends Semagram[D] {
  type Model = A

  type Data = D


  def stateMsg(es:EditorState,a:A): Message[A] = Message()



  // val execute(msg:Message[A]): IO[A]
  
  /** Construct a `SemagramElt` from a collection of `Binding` interactions
   *  and an optional initial value
   */ 
  def apply(bindings: Seq[Binding[A]],modelVar:Var[Model]): SemagramElt[D,A] = {
    
    /* Construct the state variables */
    // val modelVar = mVar 
    val stateVar = Var(EditorState(None,Complex(1,1),Complex(0,0),Seq(),Set())) 
    val messageBus: EventBus[Either[Message[Model],Message[EditorState]]] = EventBus()
    val messageObs = Observer[Either[Message[Model],Message[EditorState]]](_ match
      case Left(msg) => modelVar.update(msg.execute)
      case Right(msg) => stateVar.update(msg.execute)
    )

    /* Construct state variable (attached to the element) and global
     *  state variable (attached to the window) */
    // val globalStateVar = Var(GlobalState(Set(),Seq()))
        
    /* Construct event buses to manipulate state variables */
    val eventBus = EventBus[Event]()
    val globalEventBus = EventBus[Event]()
    

    /* Modify model to account for local state (e.g., hover) */ 
    val statefulSig = stateVar.signal.combineWith(modelVar.signal)
      .map((state,acset) => stateMsg(state,acset).execute(acset))

    // I built messages into the `Event` structure (`MsgEvent`) before I 
    // really understood the laminar API with a `MsgEvent`.
    // Refactor to use it or remove?
    // val outbox = EventBus[Either[Message[Model],Message[EditorState]]]()

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
      this.apply(statefulSig,eventBus.writer).amend(
        svg.height := "100%",
        svg.width := "100%",
      ),

      messageBus --> messageObs,
      globalEventBus.events --> eventBus.writer,
      defaultAttrs,
      inContext(thisNode =>  
        onMountCallback(_ => stateVar.update(_.copy(
          dims = Complex(thisNode.ref.clientWidth,thisNode.ref.clientHeight)
        ))
      )),
    )

    dom.window.addEventListener("resize",_ => stateVar.update{state => 
      state.copy(
        dims = Complex(semaElt.ref.clientWidth,semaElt.ref.clientHeight)
      )
    })

    

    /* Attach global (keyboard) listeners */
    EditorState.listen(globalEventBus.writer)

    /* Start cats.io event queue */
    val main = for {
      eventQueue <- Queue.unbounded[IO, Event]
      _ <- Dispatcher.sequential[IO] use { dispatcher =>
        semaElt.amend(
          eventBus.events --> Observer[Event](evt =>
            dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
          )
        )
        Binding.processAll(Action.Resources(modelVar, stateVar, eventQueue, messageBus.writer), bindings)
      }
    } yield ()

    main.unsafeRunAndForget()(unsafe.IORuntime.global)

    /* Return associated `SemagramElt` */
    SemagramElt(
      semaElt,
      () => (modelVar.now(),stateVar.now()),
      statefulSig.combineWith(stateVar.signal),
      messageBus
    )
  }


}