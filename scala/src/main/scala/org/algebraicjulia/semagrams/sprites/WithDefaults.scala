package org.algebraicjulia.semagrams.sprites

import com.raquo.laminar.api.L._
import org.algebraicjulia.semagrams._

case class WithDefaults(defaults: PropMap) extends Middleware {
  override def updateProps(ent: Entity, p: PropMap) = defaults ++ p
}
