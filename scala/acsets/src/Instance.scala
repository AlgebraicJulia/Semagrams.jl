package acsets

import java.util.UUID

case class MethodApp(
    part: PartId,
    prop: PropId
)

given Ordering[MethodApp] = new Ordering[MethodApp] {
  val partOrdering: Ordering[PartId] = Ordering[PartId]
  val propOrdering: Ordering[PropId] = Ordering[PropId]
  def compare(x: MethodApp, y: MethodApp) = {
    val partCmp = partOrdering.compare(x.part, y.part)
    if partCmp == 0 then propOrdering.compare(x.prop, y.prop)
    else partCmp
  }
}

trait Instance {
  def schema: Schema
  def parts: Map[PartId, SortId]
  def subparts: Map[MethodApp, Value]

  def subpart(part: Part, prop: PropId): Option[Value] =
    subparts.get(MethodApp(part.id, prop))

  def subpart(part: Part, prop: Property): Option[prop.T] =
    subpart(part, prop.id).map(v => prop.coerce(v).get)

  def sort(ent: PartId): Option[SortId] = parts.get(ent)
}

object PartDelta extends MapDelta[PartId, SortId]
object SubpartDelta extends MapDelta[MethodApp, Value]

object Instance extends Delta[Instance] {
  case class Patch(
      schema: Schema,
      parts: PartDelta.Patch,
      subparts: SubpartDelta.Patch
  ) {
    def addPart(clean: Clean, sort: SortId): (Patch, Part) = {
      if (!(schema.sorts contains sort)) {
        throw (new Exception(s"schema does not contain sort ${sort}"))
      }
      val id = UUID.randomUUID()
      (this.copy(parts = parts + (id -> sort)), Part(id, sort))
    }

    def remPart(clean: Clean, part: Part): Patch = {
      if (clean.parts contains part.id) {
        this.copy(parts = parts - part.id)
      } else {
        this
      }
    }

    def setSubpart(clean: Clean, part: Part, prop: Property, x: prop.T): Patch =
      setSubpart(clean, part, prop.id, prop produce x)

    def setSubpart(clean: Clean, part: Part, prop: PropId, v: Value): Patch = {
      if (!(schema.props contains prop)) {
        throw (new Exception(s"schema does not contain property ${prop}"))
      }
      val ty = schema.props(prop)
      if (!(ty coerce v).isDefined) {
        throw (new Exception(
          s"value ${v} does not match type for property ${prop}"
        ))
      }
      if (!(schema.methods contains Method(part.sort, prop))) {
        throw (new Exception(
          s"schema does not contain a method for ${prop} on sort ${part.sort}"
        ))
      }
      this.copy(subparts = subparts + (MethodApp(part.id, prop) -> v))
    }
  }

  object Patch {
    def empty(schema: Schema) =
      Patch(schema, PartDelta.Patch.empty, SubpartDelta.Patch.empty)
  }

  class Clean(
      val schema: Schema,
      val parts: PartDelta.Clean,
      val subparts: SubpartDelta.Clean
  ) extends Instance {}

  object Clean {
    def empty(schema: Schema): Clean = {
      new Clean(schema, PartDelta.Clean.empty, SubpartDelta.Clean.empty)
    }
  }

  case class Dirty(
      clean: Clean,
      patch: Patch
  ) extends Instance {
    if (clean.schema != patch.schema) {
      throw (new Exception("incompatible schemas"))
    }
    def schema = clean.schema
    def parts: PartDelta.Dirty =
      PartDelta.Dirty(clean.parts, patch.parts)
    def subparts: SubpartDelta.Dirty =
      SubpartDelta.Dirty(clean.subparts, patch.subparts)

    def addPart(sort: SortId) = {
      val (newPatch, part) = patch.addPart(clean, sort)
      (this.copy(patch = newPatch), part)
    }

    def remPart(part: Part) =
      this.copy(patch = patch.remPart(clean, part))

    def setSubpart(part: Part, prop: PropId, v: Value): Dirty =
      this.copy(patch = patch.setSubpart(clean, part, prop, v))

    def setSubpart(part: Part, prop: Property, x: prop.T): Dirty =
      this.copy(patch = patch.setSubpart(clean, part, prop, x))
  }

  object Dirty {
    def empty(schema: Schema) = Dirty(Clean.empty(schema), Patch.empty(schema))
  }

  def commit(state: Dirty): Option[Patch] = for {
    parts <- PartDelta.commit(state.parts)
    subparts <- SubpartDelta.commit(state.subparts)
  } yield Patch(state.clean.schema, parts, subparts)

  def applyPatch(state: Clean, patch: Patch): Option[Clean] = for {
    parts <- PartDelta.applyPatch(state.parts, patch.parts)
    subparts <- SubpartDelta.applyPatch(
      state.subparts,
      patch.subparts
    )
  } yield new Clean(state.schema, parts, subparts)
}
