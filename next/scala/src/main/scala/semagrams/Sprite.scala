package semagrams

import semagrams.Entity
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement

/** A trait representing a sprite that can communicate by sending events
 */
trait Sprite {
  type Model

  def present(
    ent: Entity,
    initModel: Model,
    updates: Signal[Model],
  ): SvgElement
}

class SpriteCollection(
  val S: Sprite,
  val sprites: Signal[List[(Entity, S.Model)]]
) extends Modifier[SvgElement] {
  def present(): Signal[List[SvgElement]] = {
    sprites.split(_._1)({ case (ent, init, updates) => S.present(ent, init._2, updates.map(_._2))})
  }

  override def apply(el: SvgElement) = el.amend(
    children <-- present()
  )
}

object SpriteCollection {
  def apply(S: Sprite, sprites: Signal[List[(Entity, S.Model)]]) = {
    new SpriteCollection(S, sprites)
  }
}
