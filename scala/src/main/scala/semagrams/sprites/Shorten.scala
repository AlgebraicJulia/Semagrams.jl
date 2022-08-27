package semagrams.sprites

import semagrams.Entity
import semagrams.util._

case class Shorten(amount: Double) extends Middleware {
  override def updateProps(ent: Entity, p: PropMap): PropMap = {
    val s = p(Start)
    val e = p(End)
    val diff = s - e
    val dir = diff / diff.abs
    p + (Start, s - (dir * amount)) + (End, e + (dir * amount))
  }
}
