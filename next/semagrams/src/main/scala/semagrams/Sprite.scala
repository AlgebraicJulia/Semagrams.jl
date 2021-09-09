package semagrams

import semagrams.ACSets._
import semagrams.Params._
import semagrams.Config

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._

object Sprite {
  enum SpriteEvent:
    case MouseOver(e: Entity)
    case MouseOut(e: Entity)

  case class SpriteConfig(
    events: L.Observer[SpriteEvent],
    global_config: Config
  )

  def createSprite(config: SpriteConfig)(
    entity: Entity,
    init: (Entity, EntityParams),
    updateSignal: L.Signal[(Entity, EntityParams)]
  ) = {
    val hovered = L.Var(false)
    circle(
      stroke := config.global_config.sprite_stroke,
      fill <-- hovered.signal.map(if _
                                    then config.global_config.sprite_hovered_fill
                                    else config.global_config.sprite_default_fill),
      cx <-- updateSignal.map(_._2.x.toString),
      cy <-- updateSignal.map(_._2.y.toString),
      r := config.global_config.sprite_radius.toString,
      L.onMouseOver --> config.events.contramap(ev => SpriteEvent.MouseOver(entity)),
      L.onMouseOut --> config.events.contramap(ev => SpriteEvent.MouseOut(entity)),
      L.onMouseOver --> hovered.writer.contramap(_ => true),
      L.onMouseOut --> hovered.writer.contramap(_ => false)
    )
  }
}
