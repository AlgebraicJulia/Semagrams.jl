package acsets.collection.immutable

import scala.collection.immutable.{HashMap, HashSet}

case class HashMapPatch[K, V](
    intros: HashMap[K, V],
    deletions: HashSet[K]
)

case class PatchedHashMap[K, V](
    clean: HashMap[K, V],
    patch: HashMapPatch[K, V]
) extends Map[K, V] {
  def iterator: Iterator[(K, V)] =
    clean.iterator.filter(kv =>
      !patch.deletions.contains(kv._1)
    ) ++ patch.intros.iterator

  def get(key: K): Option[V] =
    patch.intros
      .get(key)
      .map(Some(_))
      .getOrElse(
        if patch.deletions.contains(key) then None else clean.get(key)
      )

  def removed(key: K): PatchedHashMap[K, V] = {
    if patch.intros contains key then
      this.copy(patch = patch.copy(intros = patch.intros.removed(key)))
    else if clean contains key then
      this.copy(patch = patch.copy(deletions = patch.deletions + key))
    else this
  }

  def updated[V1 >: V](k: K, v: V1): PatchedHashMap[K, V1] =
    PatchedHashMap[K, V1](
      clean,
      HashMapPatch(patch.intros.updated(k, v), patch.deletions - k)
    )
}
