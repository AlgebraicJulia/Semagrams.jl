package acsets.collection.immutable

import scala.collection.immutable.HashSet

case class HashSetPatch[K](
    intros: HashSet[K],
    deletions: HashSet[K]
)

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
    if patch.intros contains elem then
      this.copy(patch = patch.copy(intros = patch.intros.excl(elem)))
    else if clean contains elem then
      this.copy(patch = patch.copy(deletions = patch.deletions + elem))
    else this

  def incl(elem: K): PatchedHashSet[K] =
    PatchedHashSet[K](
      clean,
      HashSetPatch(patch.intros.incl(elem), patch.deletions - elem)
    )
}
