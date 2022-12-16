package semagrams.sprites

import semagrams._

def GenericEntitySource() =
  EntitySource[Seq[(Entity, Sprite, PropMap)]]((entities, _) => entities)
