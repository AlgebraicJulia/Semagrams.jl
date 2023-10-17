package acsets

import scala.collection.immutable.{HashMap, HashSet}
import acsets.collection.immutable.{
  PatchedHashMap,
  HashMapPatch,
  PatchedHashSet,
  HashSetPatch
}

/** The lifecycle of a patch:
  *
  *   - Start as a "working area" patch
  *   - Entities and definitions are added to it incrementally
  *   - Can be "applied" to any Instantiation, to get a temporary Instantiation
  *   - Finally, is "frozen" against a specific set of parents, against which it
  *     is validated.
  */
trait Delta[T] {
  type Patch

  type Clean <: T

  type Dirty <: T

  def commit(state: Dirty): Option[Patch]

  def applyPatch(state: Clean, patch: Patch): Option[Clean]
}

trait MapDelta[K: Ordering, V] extends Delta[Map[K, V]] {
  type Patch = HashMapPatch[K, V]

  type Clean = HashMap[K, V]
  val Clean = HashMap

  type Dirty = PatchedHashMap[K, V]
  val Dirty = PatchedHashMap

  /** TODO: check intros/deletions */
  def commit(state: Dirty) = Some(state.patch)

  /** TODO: check intros/deletions */
  def applyPatch(state: Clean, patch: Patch) =
    Some(state -- patch.deletions ++ patch.intros)
}

trait SetDelta[K: Ordering] extends Delta[Set[K]] {
  type Patch = HashSetPatch[K]

  type Clean = HashSet[K]
  val Clean = HashSet

  type Dirty = PatchedHashSet[K]
  val Dirty = PatchedHashSet

  def commit(state: Dirty) = Some(state.patch)

  def applyPatch(state: Clean, patch: Patch) =
    Some(state -- patch.deletions ++ patch.intros)
}
