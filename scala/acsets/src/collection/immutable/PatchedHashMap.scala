package acsets.collection.immutable

import scala.collection.immutable.{HashMap, HashSet}

case class HashMapPatch[K, V](
    intros: HashMap[K, V],
    deletions: HashSet[K]
) {
  def +(kv: (K, V)) = this.copy(intros + kv, deletions)

  def -(k: K) = this.copy(intros - k, deletions + k)

  def get(clean: HashMap[K, V], key: K): Option[V] =
    intros
      .get(key)
      .map(Some(_))
      .getOrElse(
        if deletions.contains(key) then None else clean.get(key)
      )

  def removed(clean: HashMap[K, V], key: K): HashMapPatch[K, V] =
    if intros contains key then this.copy(intros = intros.removed(key))
    else if clean contains key then this.copy(deletions = deletions + key)
    else this

  def updated[V1 >: V](clean: HashMap[K, V], k: K, v: V1): HashMapPatch[K, V1] =
    HashMapPatch[K, V1](intros.updated(k, v), deletions - k)
}

object HashMapPatch {
  def empty[K, V] = HashMapPatch[K, V](HashMap(), HashSet())
}

case class PatchedHashMap[K, V](
    clean: HashMap[K, V],
    patch: HashMapPatch[K, V]
) extends Map[K, V] {
  def iterator: Iterator[(K, V)] =
    clean.iterator.filter(kv =>
      !patch.deletions.contains(kv._1)
    ) ++ patch.intros.iterator

  def get(key: K): Option[V] = patch.get(clean, key)

  def removed(key: K): PatchedHashMap[K, V] =
    this.copy(patch = patch.removed(clean, key))

  def updated[V1 >: V](k: K, v: V1): PatchedHashMap[K, V1] =
    PatchedHashMap[K, V1](
      clean,
      patch.updated(clean, k, v)
    )
}
