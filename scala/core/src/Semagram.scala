package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

import org.scalajs.dom

import cats.effect._
import cats.effect.std._

import semagrams.acsets._
import semagrams.state._
import semagrams.rendering.{EntitySource,EntityCollector}
import semagrams.bindings._
import semagrams.util._

import scala.annotation.targetName
// import semagrams.{Ob, Property, PropChange}







type StateMsg[Model] = Either[Message[Model],Message[EditorState]]




trait Semagram[Model,D:PartData]:
  /* Implementation API */
  type DisplayModel = Model

  /** Computes dynamically some layout properties. */
  def layout(m:Model,es:EditorState): DisplayModel

  /** Extractors for the various entities in the Semagram */
  val entitySources: Seq[EntitySource[DisplayModel,D]]

  val modelVar: UndoableVar[Model]

  def bindings: Seq[Binding[Model]]




  def readout() = (stateVar.now(),modelVar.now())

  
  def produceEntities(dm: DisplayModel): Seq[(Part,(Sprite[D],D))] =
    EntityCollector.collect(dm, entitySources)



  val stateVar = Var(EditorState()) 

  val modelSig = modelVar.signal
  
  val stateModelSig = 
    modelSig.combineWith(stateVar.signal).map(layout)

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
  

  val defaultAttrs = Seq(
    backgroundColor := "white",
    height := "400px",
    width := "100%",
    border := "black",
    borderStyle := "solid",
    boxSizing := "border-box",
  )
      
  /* Construct the laminar element associated with the semagram */
  val elt = div(
    cls := "semagram-element",
    defaultAttrs,
    svg.svg(
      svg.cls := "semagram-svg",
      util.svgDefs(),
      svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
      children <-- stateModelSig.map(produceEntities)
        .split(_._1){ case (part,(_,(sprite,init)),kvSig) =>
          sprite.present(part,init,kvSig.map(_._2._2),eventBus.writer)
        },
      mouseMoveListener(eventBus.writer),
      MouseEvents.handlers(Background, eventBus.writer),
      svg.height := "100%",
      svg.width := "100%",
    ),
    messageBus --> messageObs,
    globalEventBus.events --> eventBus.writer,
    inContext(thisNode => onMountCallback(_ => stateVar.update(_.copy(
      dims = Complex(thisNode.ref.clientWidth,thisNode.ref.clientHeight)
    )))),
  )

  dom.window.addEventListener("resize",_ => stateVar.update{state => 
    state.copy(
      dims = Complex(elt.ref.clientWidth,elt.ref.clientHeight)
    )
  })

      

  /* Attach global (keyboard) listeners */
  EditorState.listen(globalEventBus.writer)

  /* Start cats.io event queue */
  val main = for {
    eventQueue <- Queue.unbounded[IO, Event]
    _ <- Dispatcher.sequential[IO] use { dispatcher =>
      elt.amend(
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

  case class EditTable(ob:Ob,cols:Seq[Property],withLayout:Boolean = true):
    def table = widgets.PropTable[Part](ob.label,cols)
    val (editMsgs,editObs) = EventStream.withObserver[(Part,Property)]
    
    val eltSig = if withLayout then stateModelSig else modelSig
    val elt = table.laminarElt(
      eltSig.map(_.getPropSeq(ob)),
      editMsgs.map(cell => widgets.EditMsg(None,Some(cell))),
      modelObs.contramap(acsetMsg)
    )
    def edit(part:Part,f:Property) = editObs.onNext(part -> f)

  def acsetMsg(msg:ChangeMsg[Part]): Message[ACSet[D]] = 
    msg match
    case SetValue(p,change) => ChangePropMsg(p,change)
    case HoverMsg(p,highlighted) => if highlighted 
      then ChangePropMsg(p,PropChange(Highlight,None,()))
      else ChangePropMsg(p,PropChange(Highlight,(),None))

  /** A callback function for passing messages externally **/
  def update(msg:Message[ACSet[D]]) = modelObs.onNext(msg)
  @targetName("updateState")
  def update(msg:Message[EditorState]) = stateObs.onNext(msg)

  def isHovered: Boolean = stateVar.now().isHovered


  val tableVar = Var(Map[UID,EditTable]())
  

  def editTable(ob:Ob,cols:Property*) = EditTable(ob,cols)






