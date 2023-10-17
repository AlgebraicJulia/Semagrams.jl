package acsets

import java.util.UUID
case class MethodApp(
    operand: EntId,
    operation: PropId
)

given Ordering[MethodApp] = new Ordering[MethodApp] {
  val uuidOrdering: Ordering[UUID] = Ordering[UUID]
  def compare(x: MethodApp, y: MethodApp) = {
    val opCmp = uuidOrdering.compare(x.operand, y.operand)
    if opCmp == 0 then uuidOrdering.compare(x.operation, y.operation)
    else opCmp
  }
}

trait Instance {
  val schema: Schema
  val entities: Map[EntId, SortId]
  val definitions: Map[MethodApp, Value]
}

object EntityDelta extends MapDelta[EntId, SortId]
object DefinitionDelta extends MapDelta[MethodApp, Value]

object Instance extends Delta[Instance] {
  case class Patch(
      schema: Schema,
      entities: EntityDelta.Patch,
      definitions: DefinitionDelta.Patch
  )

  class Clean(
      val schema: Schema,
      val entities: EntityDelta.Clean,
      val definitions: DefinitionDelta.Clean
  ) extends Instance

  case class Dirty(
      clean: Clean,
      patch: Patch
  ) extends Instance {
    if (clean.schema != patch.schema) {
      throw (new Exception("incompatible schemas"))
    }
    val schema = clean.schema
    val entities: EntityDelta.Dirty =
      EntityDelta.Dirty(clean.entities, patch.entities)
    val definitions: DefinitionDelta.Dirty =
      DefinitionDelta.Dirty(clean.definitions, patch.definitions)
  }

  def commit(state: Dirty): Option[Patch] = for {
    entities <- EntityDelta.commit(state.entities)
    definitions <- DefinitionDelta.commit(state.definitions)
  } yield Patch(state.clean.schema, entities, definitions)

  def applyPatch(state: Clean, patch: Patch): Option[Clean] = for {
    entities <- EntityDelta.applyPatch(state.entities, patch.entities)
    definitions <- DefinitionDelta.applyPatch(
      state.definitions,
      patch.definitions
    )
  } yield new Clean(state.schema, entities, definitions)
}
