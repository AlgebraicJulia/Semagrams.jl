package acsets

import java.util.UUID

case class Method(
    sort: SortId,
    prop: PropId
)

given Ordering[Method] = new Ordering[Method] {
  val uuidOrdering: Ordering[UUID] = Ordering[UUID]
  def compare(x: Method, y: Method) = {
    val sortCmp = uuidOrdering.compare(x.sort, y.sort)
    if sortCmp == 0 then uuidOrdering.compare(x.prop, y.prop) else sortCmp
  }
}

trait Schema {
  val sorts: Set[SortId]
  val props: Map[PropId, ValueType]
  val methods: Set[Method]
}

object SortDelta extends SetDelta[SortId]
object PropDelta extends MapDelta[PropId, ValueType]
object MethodDelta extends SetDelta[Method]

object Schema extends Delta[Schema] {
  case class Patch(
      sorts: SortDelta.Patch,
      props: PropDelta.Patch,
      methods: MethodDelta.Patch
  )

  class Clean(
      val sorts: SortDelta.Clean,
      val props: PropDelta.Clean,
      val methods: MethodDelta.Clean
  ) extends Schema

  case class Dirty(
      clean: Clean,
      patch: Patch
  ) extends Schema {
    val sorts: SortDelta.Dirty = SortDelta.Dirty(clean.sorts, patch.sorts)
    val props: PropDelta.Dirty = PropDelta.Dirty(clean.props, patch.props)
    val methods: MethodDelta.Dirty =
      MethodDelta.Dirty(clean.methods, patch.methods)
  }

  def commit(state: Dirty): Option[Patch] = for {
    sorts <- SortDelta.commit(state.sorts)
    props <- PropDelta.commit(state.props)
    methods <- MethodDelta.commit(state.methods)
  } yield Patch(sorts, props, methods)

  def applyPatch(state: Clean, patch: Patch) = for {
    sorts <- SortDelta.applyPatch(state.sorts, patch.sorts)
    props <- PropDelta.applyPatch(state.props, patch.props)
    methods <- MethodDelta.applyPatch(state.methods, patch.methods)
  } yield new Clean(sorts, props, methods)
}
