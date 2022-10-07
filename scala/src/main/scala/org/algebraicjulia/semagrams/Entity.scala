package org.algebraicjulia.semagrams

abstract class EntityType

abstract class Entity {
  def entityType: EntityType
  val id: Int
}
