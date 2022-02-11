package semagrams

import com.raquo.laminar.api.L._
import semagrams.Sprite._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec
import semagrams.util.Complex
import scala.collection.mutable
import cats._

case class EditorState[Model, InitData](
  model: Model,
  reg: ControllerRegistry,
  initData: InitData
)

/** A trait representing an editor
 */
trait SemagramsEditor {
  type Model
  type InitData

  def updateModel(state: EditorState[Model, InitData], e: EditorEvent): Outcome[Model]

  def spriteCollections(initData: InitData, $model: Signal[Model]): List[SpriteCollection]

  def initialize(model: Model): (InitData, ControllerRegistry)

  def update(
    state: EditorState[Model, InitData],
    e: EditorEvent
  ): EditorState[Model, InitData] = {
    val stack = mutable.Stack[EditorEvent]()
    var curState = state
    stack.push(e)
    while (!stack.isEmpty) {
      val evt = stack.pop()
      val res = for {
        newReg <- curState.reg.processEvent(evt)
        newModel <- updateModel(curState.copy(reg = newReg), evt)
      } yield curState.copy(model = newModel, reg = newReg)
      res.run match {
        case Right((events, newState)) => {
          stack.pushAll(events)
          curState = newState
        }
        case Left(msg) => {
          println("Error: " + msg)
        }
      }
    }
    curState
  }

  def start(initModel: Model): SvgElement = {
    val bus = EventBus[EditorEvent]()
    val (initData, initReg) = initialize(initModel)
    val state = Var(EditorState(initModel, initReg, initData))
    val scs = spriteCollections(initData, state.signal.map(_.model))

    val el = svg.svg(
      svg.width := "400",
      svg.height := "400",
      svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
      bus.events --> state.updater(update),
    )

    initReg.globalHooks(el, state.signal.map(_.reg), bus.writer)

    for (sc <- scs) {
      el.amend(children <-- sc.present(state.signal.map(_.reg), bus.writer))
    }

    return el
  }
}
