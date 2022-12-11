package semagrams.sprites

import semagrams._
import semagrams.acsets._
import com.raquo.laminar.api.L._

class ACSetEntitySource[Schema: IsSchema](
  val $acset: Signal[ACSet[Schema]],
  val ob: Ob,
  val sprite: Sprite
) extends EntitySource {
  def entities(_m: Signal[EntityMap]): Signal[EntityMap] =
    $acset.map(acs => acs.parts(ob).map(i => (i, (sprite, acs.props(i)))).toMap)
}

class DynamicEntitySource(
  val $em: Var[EntityMap]
) extends EntitySource {
  def entities(_m: Signal[EntityMap]) = $em.signal
}
