package acsets

case class Method(
    sort: SortId,
    prop: PropId
)

given Ordering[Method] = new Ordering[Method] {
  val sortOrdering: Ordering[SortId] = Ordering[SortId]
  val propOrdering: Ordering[PropId] = Ordering[PropId]
  def compare(x: Method, y: Method) = {
    val sortCmp = sortOrdering.compare(x.sort, y.sort)
    if sortCmp == 0 then propOrdering.compare(x.prop, y.prop) else sortCmp
  }
}

trait Schema {
  def sorts: Set[SortId]
  def props: Map[PropId, ValueType]
  def methods: Set[Method]
}

object SortDelta extends SetDelta[SortId]
object PropDelta extends MapDelta[PropId, ValueType]
object MethodDelta extends SetDelta[Method]

object Schema extends Delta[Schema] {
  case class Patch(
      sorts: SortDelta.Patch,
      props: PropDelta.Patch,
      methods: MethodDelta.Patch
  ) {
    def addSort(clean: Clean, sort: SortId) = {
      this.copy(sorts = sorts.incl(clean.sorts, sort))
    }

    def addProp(clean: Clean, prop: PropId, vtype: ValueType) = {
      this.copy(props = props.updated(clean.props, prop, vtype))
    }

    def addMethod(clean: Clean, sort: SortId, prop: PropId) = {
      this.copy(methods = methods.incl(clean.methods, Method(sort, prop)))
    }
  }

  object Patch {
    def empty = Patch(
      SortDelta.Patch.empty,
      PropDelta.Patch.empty,
      MethodDelta.Patch.empty
    )
  }

  class Clean(
      val sorts: SortDelta.Clean,
      val props: PropDelta.Clean,
      val methods: MethodDelta.Clean
  ) extends Schema

  object Clean {
    def empty = Clean(
      SortDelta.Clean.empty,
      PropDelta.Clean.empty,
      MethodDelta.Clean.empty
    )
  }

  case class Dirty(
      clean: Clean,
      patch: Patch
  ) extends Schema {
    def sorts: SortDelta.Dirty = SortDelta.Dirty(clean.sorts, patch.sorts)
    def props: PropDelta.Dirty = PropDelta.Dirty(clean.props, patch.props)
    def methods: MethodDelta.Dirty =
      MethodDelta.Dirty(clean.methods, patch.methods)
    def addSort(sort: SortId) = Dirty(clean, patch.addSort(clean, sort))

    def addProp(prop: PropId, vtype: ValueType) =
      Dirty(clean, patch.addProp(clean, prop, vtype))

    def addMethod(sort: SortId, prop: PropId) =
      Dirty(clean, patch.addMethod(clean, sort, prop))

    def applyPatch(): Option[Clean] = Schema.applyPatch(clean, patch)
  }

  object Dirty {
    def apply(): Dirty = apply(Clean.empty)
    def apply(clean: Clean): Dirty = Dirty(clean, Patch.empty)
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
