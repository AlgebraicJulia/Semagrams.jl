package acsets.collection.mutable

import scala.collection.immutable
import scala.collection.mutable

case class HashMapPatch[K, V](
    intros: mutable.HashMap[K, V],
    deletions: mutable.HashSet[K]
) {
  def get(clean: immutable.Map[K, V], key: K): Option[V] =
    intros
      .get(key)
      .map(Some(_))
      .getOrElse(
        if deletions.contains(key) then None else clean.get(key)
      )

  def addOne(elem: (K, V)): this.type = {
    intros.addOne(elem)
    this
  }

  def subtractOne(clean: immutable.Map[K, V], k: K): this.type = {
    if (clean contains k) {
      deletions.addOne(k)
      intros.subtractOne(k)
    } else {
      intros.subtractOne(k)
    }
    this
  }
}

case class PatchedMap[K, V, M <: immutable.Map[K, V]](
    clean: M,
    patch: HashMapPatch[K, V]
) extends mutable.Map[K, V] {
  def iterator: Iterator[(K, V)] =
    clean.iterator.filter(kv =>
      !patch.deletions.contains(kv._1)
    ) ++ patch.intros.iterator

  def get(key: K): Option[V] = patch.get(clean, key)

  def addOne(elem: (K, V)): this.type = {
    patch.addOne(elem)
    this
  }

  def subtractOne(k: K): this.type = {
    patch.subtractOne(clean, k)
    this
  }
}
