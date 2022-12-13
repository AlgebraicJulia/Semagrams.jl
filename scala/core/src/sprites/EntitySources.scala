package semagrams.sprites

import semagrams._
import semagrams.acsets._
import com.raquo.laminar.api.L._

def ACSetEntitySource[Schema: IsSchema](
    $acset: Signal[ACSet[Schema]],
    ob: Ob,
    sprite: Sprite
) = EntitySource(_m =>
  $acset.map(acs => acs.parts(ob).map(i => (i, (sprite, acs.props(i)))).toMap)
)

def DynamicEntitySource(
    $em: Var[EntityMap]
) = EntitySource(_m => $em.signal)
