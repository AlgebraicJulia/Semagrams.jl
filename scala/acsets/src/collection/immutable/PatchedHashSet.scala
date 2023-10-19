package acsets.collection.immutable

import scala.collection.immutable.HashSet

case class HashSetPatch[K](
    intros: HashSet[K],
    deletions: HashSet[K]
) {
  def +(k: K) = HashSetPatch[K](intros + k, deletions - k)
  def -(k: K) = HashSetPatch[K](intros - k, deletions + k)

  def incl(clean: HashSet[K], elem: K) =
    if clean contains elem then this else this + elem

  def excl(clean: HashSet[K], elem: K) =
    if clean contains elem then this - elem else this
}

object HashSetPatch {
  def empty[K] = HashSetPatch[K](HashSet(), HashSet())
}

case class PatchedHashSet[K](
    clean: HashSet[K],
    patch: HashSetPatch[K]
) extends Set[K] {
  def iterator: Iterator[K] =
    clean.iterator.filter(k =>
      !patch.deletions.contains(k)
    ) ++ patch.intros.iterator

  def contains(elem: K) = !(patch.deletions
    .contains(elem)) && (patch.intros.contains(elem) || clean.contains(elem))

  def excl(elem: K) =
    this.copy(
      patch = patch.incl(clean, elem)
    )

  def incl(elem: K): PatchedHashSet[K] =
    this.copy(
      patch = patch.excl(clean, elem)
    )
}
