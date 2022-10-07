package org.algebraicjulia.semagrams.sprites

import org.algebraicjulia.semagrams.Entity
import org.algebraicjulia.semagrams.util._

case class Shorten(amount: Double) extends Middleware {
  override def updateProps(ent: Entity, p: PropMap): PropMap = {
    val s = p(Start)
    val e = p(End)
    val diff = s - e
    val dir = diff / diff.abs
    val rot = Complex(0, p(Bend)).exp
    p + (Start, s - (rot * dir * amount)) + (End, e + (rot.cong * dir * amount))
  }
}
