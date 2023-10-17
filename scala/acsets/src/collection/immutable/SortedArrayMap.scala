package acsets.collection.immutable

import scala.collection.immutable.{
  ArraySeq,
  SortedMap,
  StrictOptimizedSortedMapOps
}
import scala.collection.SortedMapFactoryDefaults
import scala.collection.AbstractMap
import scala.collection.SortedMapFactory
import scala.collection.generic.DefaultSerializable
import scala.collection.Searching._
import scala.collection.mutable.ReusableBuilder
import scala.collection.mutable.ArrayBuffer

final class SortedArrayMap[K, +V] private (private val vals: ArraySeq[(K, V)])(
    implicit val ordering: Ordering[K]
) extends AbstractMap[K, V]
    with SortedMap[K, V]
    with StrictOptimizedSortedMapOps[K, V, SortedArrayMap, SortedArrayMap[K, V]]
    with SortedMapFactoryDefaults[K, V, SortedArrayMap, Iterable, Map]
    with DefaultSerializable {
  def this()(implicit ordering: Ordering[K]) = this(ArraySeq())(ordering)

  private[this] def newMapOrSelf[V1 >: V](
      newVals: ArraySeq[(K, V1)]
  ): SortedArrayMap[K, V1] =
    if (newVals eq vals)
      this
    else
      new SortedArrayMap[K, V1](newVals)

  override def sortedMapFactory: SortedMapFactory[SortedArrayMap] =
    SortedArrayMap

  def iterator: Iterator[(K, V)] = vals.iterator

  def binarySearch(key: K, from: Int, to: Int): SearchResult = {
    if (to <= from) InsertionPoint(from)
    else {
      val idx = from + (to - from - 1) / 2
      val (k, _) = vals(idx)
      math.signum(ordering.compare(key, k)) match {
        case -1 => binarySearch(key, from, idx)
        case 1  => binarySearch(key, idx + 1, to)
        case _  => Found(idx)
      }
    }
  }

  def binarySearch(key: K): SearchResult = binarySearch(key, 0, vals.length)

  def get(key: K): Option[V] = binarySearch(key) match {
    case Found(i)          => Some(vals(i)._2)
    case InsertionPoint(_) => None
  }

  def removed(key: K): SortedArrayMap[K, V] = binarySearch(key) match {
    case Found(i) =>
      new SortedArrayMap[K, V](
        vals.slice(0, i) ++ vals.slice(i + 1, vals.length)
      )
    case InsertionPoint(_) => this
  }

  def updated[V1 >: V](key: K, value: V1): SortedArrayMap[K, V1] = binarySearch(
    key
  ) match {
    case Found(i) => newMapOrSelf(vals.updated(i, (key, value)))
    case InsertionPoint(i) =>
      new SortedArrayMap[K, V1](
        (vals.slice(0, i) :+ (key, value)) ++ vals.slice(i, vals.length)
      )
  }

  def iteratorFrom(key: K) = binarySearch(key) match {
    case Found(i)          => vals.iterator.drop(i - 1)
    case InsertionPoint(i) => vals.iterator.drop(i - 1)
  }

  def keysIteratorFrom(start: K): Iterator[K] = iteratorFrom(start).map(_._1)

  def rangeImpl(from: Option[K], until: Option[K]): SortedArrayMap[K, V] = {
    val s = from.map(binarySearch(_).insertionPoint).getOrElse(0)
    val e = until.map(binarySearch(_).insertionPoint).getOrElse(vals.length)
    newMapOrSelf(vals.slice(s, e))
  }
}

object SortedArrayMap extends SortedMapFactory[SortedArrayMap] {
  def empty[K: Ordering, V]: SortedArrayMap[K, V] = new SortedArrayMap()

  def from[K, V](it: IterableOnce[(K, V)])(implicit
      ordering: Ordering[K]
  ): SortedArrayMap[K, V] =
    new SortedArrayMap(ArraySeq.from(it.toArray.sortBy(_._1)))

  def newBuilder[K, V](implicit ordering: Ordering[K]) = new SortedMapBuilder()

  private class SortedMapBuilder[K, V](implicit ordering: Ordering[K])
      extends ReusableBuilder[(K, V), SortedArrayMap[K, V]] {
    private var vals: ArrayBuffer[(K, V)] = ArrayBuffer()

    def addOne(elem: (K, V)): this.type = {
      vals.addOne(elem)
      this
    }

    override def addAll(xs: IterableOnce[(K, V)]): this.type = {
      vals.addAll(xs)
      this
    }

    override def clear(): Unit = {
      vals.clear()
    }

    override def result(): SortedArrayMap[K, V] =
      new SortedArrayMap(ArraySeq.from(vals.sortInPlaceBy(_._1)))
  }
}
