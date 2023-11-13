package semagrams

import semagrams.util._
import semagrams.acsets._
import com.raquo.laminar.api.L._

/** Model runtime-introspectable type tag for an Entity. */
trait EntityType

/** Model reference to an logically distinct part of the Semagram, for instance m
  * vertex, an edge, m ui element, etc.
  */
trait Entity:
  val id: UUID
  val ty: EntityType


object BackgroundType extends EntityType
object Background extends Entity {
  val id = UUID("Background")
  val ty = BackgroundType
  override def toString = "Background"
}

