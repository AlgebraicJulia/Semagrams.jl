package semagrams.sprites

import semagrams._
import semagrams.acsets._

def GenericEntitySource() =
  EntitySource[Seq[(Entity, Sprite, ACSet)]]((entities, _) => entities)
