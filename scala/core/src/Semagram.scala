package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

import org.scalajs.dom

import cats.effect._
import cats.effect.std._

import semagrams.acsets._
import semagrams.state._
import semagrams.rendering._
import semagrams.bindings._
import semagrams.util._
// import semagrams.listeners._
// import semagrams.sprites._
// import semagrams.util._
// import semagrams.widgets._
// import semagrams.bindings.Binding
// import semagrams.bindings.Action
// import semagrams.widgets.PropTable
// import semagrams.acsets.abstr._
import scala.annotation.targetName







type StateMsg[Model] = Either[Message[Model],Message[EditorState]]

  // /** Creates the svg element ready to be inserted into a larger app. */
  // def oldSemagramApply(mSig: Signal[(Model,EditorState)], eventWriter: Observer[Event]): SvgElement = {
  //   svg.svg(
  //     util.svgDefs(),
  //     svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
  //     children <-- mSig.map(layout)
  //       .map(produceSprites)
  //       .split(_._1){ case (part,(_,(sprite,init)),kvSig) =>
  //         sprite.present(part,init,kvSig.map(_._2._2),eventWriter)
  //       },
  //     mouseMoveListener(eventWriter),
  //     MouseEvents.handlers(Background, eventWriter),
  //   )
  // }



// case class SemagramElt[Model,D:PartData](  
//   elt: Div,
//   readout: () => (A,EditorState),
//   signal: Signal[(A,EditorState)],
//   messageBus: EventBus[StateMsg[A]]
// ):




trait Semagram[Model,D:PartData]:
  /* Implementation API */
  type DisplayModel = Model

  /** Computes dynamically some layout properties. */
  def layout(m:Model,es:EditorState): DisplayModel

  /** Extractors for the various entities in the Semagram */
  val spriteSources: Seq[SpriteSource[DisplayModel,D]]

  val modelVar: UndoableVar[Model]

  def bindings: Seq[Binding[Model]]






  
  def produceSprites(dm: DisplayModel): Seq[(Part,(Sprite[D],D))] =
    EntityCollector.collect(dm, spriteSources)



  val stateVar = Var(EditorState()) 

  val modelSig = modelVar.signal
    .combineWith(stateVar.signal)
    .map(layout)

  val messageBus: EventBus[Either[Message[Model],Message[EditorState]]] = EventBus()
  val messageObs = Observer[Either[Message[Model],Message[EditorState]]](_ match
    case Left(msg) => modelVar.update(msg.execute)
    case Right(msg) => stateVar.update(msg.execute)
  )

  val modelObs = Observer[Message[Model]](msg =>
    messageBus.writer.onNext(Left(msg))  
  )
  val stateObs = Observer[Message[EditorState]](msg =>
    messageBus.writer.onNext(Right(msg))  
  )

  /* Construct event buses to manipulate state variables */
  val eventBus = EventBus[Event]()
  val globalEventBus = EventBus[Event]()
  

  /* Modify model to account for local state (e.g., hover) */ 
  // val statefulSig = modelVar.signal
  //   .combineWith(stateVar.signal)
  //   .map(layout)


  val defaultAttrs = Seq(
    backgroundColor := "lightblue",
    height := "400px",
    width := "100%",
    border := "black",
    borderStyle := "solid",
    boxSizing := "border-box",
  )
      
  /* Construct the laminar element associated with the semagram */
  val laminarElt = div(
    cls := "semagram-element",
    svg.svg(
      svg.cls := "semagram-svg",
      util.svgDefs(),
      svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
      children <-- modelSig.map(produceSprites)
        .split(_._1){ case (part,(_,(sprite,init)),kvSig) =>
          sprite.present(part,init,kvSig.map(_._2._2),eventBus.writer)
        },
      mouseMoveListener(eventBus.writer),
      MouseEvents.handlers(Background, eventBus.writer),
      svg.height := "100%",
      svg.width := "100%",
    ),
    // messageBus --> Observer(println),
    messageBus --> messageObs,
    globalEventBus.events --> eventBus.writer,
    defaultAttrs,
    inContext(thisNode => onMountCallback(_ => stateVar.update(_.copy(
      dims = Complex(thisNode.ref.clientWidth,thisNode.ref.clientHeight)
    )))),
  )

  dom.window.addEventListener("resize",_ => stateVar.update{state => 
    state.copy(
      dims = Complex(laminarElt.ref.clientWidth,laminarElt.ref.clientHeight)
    )
  })

      

  /* Attach global (keyboard) listeners */
  EditorState.listen(globalEventBus.writer)

  /* Start cats.io event queue */
  val main = for {
    eventQueue <- Queue.unbounded[IO, Event]
    _ <- Dispatcher.sequential[IO] use { dispatcher =>
      laminarElt.amend(
        eventBus.events --> Observer[Event](evt =>
          dispatcher.unsafeRunAndForget(eventQueue.offer(evt))
        )
      )
      Binding.processAll(Action.Resources(modelVar, stateVar, eventQueue, messageBus.writer), bindings)
    }
  } yield ()

  main.unsafeRunAndForget()(unsafe.IORuntime.global)


trait ACSemagram[D:PartData] extends Semagram[ACSet[D],D]:
  import widgets._
  case class EditTable(ob:Ob,cols:Seq[Property]):
    def table = widgets.PropTable[Part](ob.label,cols)
    val (editMsgs,editObs) = EventStream.withObserver[(Part,Property)]
    
    val elt = table.laminarElt(
      modelSig.map(_.getPropSeq(ob)),
      editMsgs.map(cell => widgets.EditMsg(None,Some(cell))),
      modelObs.contramap(acsetMsg)
    )
    def edit(part:Part,f:Property) = editObs.onNext(part -> f)

  def acsetMsg(msg:ChangeMsg[Part]): Message[ACSet[D]] = 
    msg match
    case SetValue(p,change) => ChangePropMsg(p,change)
    case HighlightMsg(p,highlighted) => if highlighted 
      then ChangePropMsg(p,PropChange(Highlight,None,()))
      else ChangePropMsg(p,PropChange(Highlight,(),None))

  /** A callback function for passing messages externally **/
  def update(msg:Message[ACSet[D]]) = modelObs.onNext(msg)
  @targetName("updateState")
  def update(msg:Message[EditorState]) = stateObs.onNext(msg)




  val tableVar = Var(Map[UUID,EditTable]())
  

  def editTable(ob:Ob,cols:Property*) = EditTable(ob,cols)









/** A class packaging the laminar element of a semagram along with
 *  the schema and accessors/modifiers
 */



  // /** Convenience method for constructing tables from semagrams **/
  // def propTable(ob:Ob,cols:Seq[Property],keys:Seq[Property]) = 
  //   SemaTable(ob,cols,keys)

  // def propTable(ob:Ob,cols:Seq[Property],key:Property): SemaTable =
  //   propTable(ob,cols,Seq(key))
  
  // def propTable(ob:Ob,cols:Property*): SemaTable =
  //   propTable(ob,cols,Seq())

  // // def propTable(ob:Ob): SemaTable =
  // //   val sch = getModel().schema
  // //   val cols = sch.homs.filter(_.dom == ob)
  // //     ++ sch.attrs.filter(_.dom == ob)
  // //   propTable(ob,cols:_*)



// /** A specialized trait for semagrams with Model == ACSet **/
// trait ACSemagram[D:PartData,A:ACSetWithData[D]] extends Semagram[D] {
  // type Model = A

  // type D = D


  // def stateMsg(es:EditorState,a:A): Message[A] = Message()



  // val execute(msg:Message[A]): IO[A]
  
  /** Construct a `SemagramElt` from a collection of `Binding` interactions
   *  and an optional initial value
   */ 

