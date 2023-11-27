package semagrams

import semagrams.util._

/** Model runtime-introspectable type tag for an Entity. */
trait EntityType

/** Model reference to an logically distinct part of the Semagram, for instance m
  * vertex, an edge, m ui element, etc.
  */
trait Entity:
  val id: UID
  val ty: EntityType


object BackgroundType extends EntityType
object Background extends Entity {
  val id = UID("Background")
  val ty = BackgroundType
  override def toString = "Background"
}

