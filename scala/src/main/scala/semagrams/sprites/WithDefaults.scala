package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._

case class WithDefaults(sprite: Sprite, defaults: PropMap) extends Sprite {
  def present(
    ent: Entity,
    init: PropMap,
    updates: Signal[PropMap]
  ) = {
    sprite.present(ent, defaults ++ init, updates.map(defaults ++ _))
  }

  def boundaryPt(
    data: PropMap,
    dir: Double
  ) = {
    sprite.boundaryPt(defaults ++ data, dir)
  }
}
