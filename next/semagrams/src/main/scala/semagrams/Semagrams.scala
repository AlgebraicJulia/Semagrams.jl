package semagrams

import semagrams.ACSets._
import semagrams.Params._
import semagrams.Sprite._
import semagrams.Config

import com.raquo.laminar.api.L._
import com.raquo.laminar.nodes.ReactiveSvgElement
import org.scalajs.dom

object Semagrams {
  enum SemagramEvent:
    case MouseDown(e: Entity)
    case MouseUp(e: Entity)

  /* TODO: this should be a free monad, in order to support arbitrarily complex commands */
  enum SemagramCommand:
    case AddEntity(ty: String, p: EntityParams)
    case RemoveEntity(e: Entity)

  case class SemagramExposed(
    hoveredEntity: Option[Entity]
  )

  case class SemagramOutput(
    events: EventStream[SemagramEvent],
    exposed: Signal[SemagramExposed]
  )

  def createSemagram(
    init_acset: ACSet[EntityParams],
    commands: EventStream[SemagramCommand],
    config: Config
  ): (SemagramOutput, ReactiveSvgElement[dom.svg.G]) = {
    val state = Var(init_acset)

    val sprite_events = EventBus[SpriteEvent]()
    val sprite_config = SpriteConfig(sprite_events.writer, config)

    val semagram_events = EventBus[SemagramEvent]()
    val exposed = Var(SemagramExposed(None))

    val output = SemagramOutput(semagram_events.events, exposed.signal)

    val sprites = state.signal
      .map(_.entities_with_params().toList)
      .splitIntoSignals(_._1)(createSprite(sprite_config))

    val display =
      svg.g(
        svg.width := "100%",
        svg.height := "100%",
        children <-- sprites,
        sprite_events.events --> exposed.updater(updateExposed),
        commands --> state.updater(updateState)
      )

    (output, display)
  }

  def updateExposed(cur: SemagramExposed, ev: SpriteEvent): SemagramExposed = {
    ev match {
      case SpriteEvent.MouseOver(e) => cur.copy(hoveredEntity = Some(e))
      case SpriteEvent.MouseOut(e) => cur.copy(hoveredEntity = None)
    }
  }

  def updateState(cur: ACSet[EntityParams], cmd: SemagramCommand): ACSet[EntityParams] = {
    cmd match {
      case SemagramCommand.AddEntity(ty, p) => cur.add_part(ty, p)._1
      case SemagramCommand.RemoveEntity(e) => cur.rem_part(e)
    }
  }
}
