package acsets.collection.immutable

import scala.collection.immutable.AbstractSet
import scala.collection.immutable.SortedSet
import scala.collection.immutable.StrictOptimizedSortedSetOps
import scala.collection.SortedSetFactoryDefaults
import scala.collection.immutable.SortedSetOps
import scala.collection.generic.DefaultSerializable
import scala.collection.SortedIterableFactory
import scala.collection.IterableOnce
import scala.collection.mutable.ReusableBuilder
import scala.collection.mutable.ArrayBuffer

final class SortedArraySet[A] private[immutable] (
    private[immutable] val map: SortedArrayMap[A, Any]
)(implicit val ordering: Ordering[A])
    extends AbstractSet[A]
    with SortedSet[A]
    with SortedSetOps[A, SortedArraySet, SortedArraySet[A]]
    with StrictOptimizedSortedSetOps[A, SortedArraySet, SortedArraySet[A]]
    with SortedSetFactoryDefaults[A, SortedArraySet, Set]
    with DefaultSerializable {

  def this()(implicit ordering: Ordering[A]) =
    this(SortedArrayMap[A, Unit]())(ordering)

  def iterator: Iterator[A] = map.iterator.map(_._1)

  def contains(elem: A) = map.get(elem).isDefined

  override def sortedIterableFactory: SortedIterableFactory[SortedArraySet] =
    SortedArraySet

  def excl(elem: A) = new SortedArraySet[A](map.removed(elem))

  def incl(elem: A) = new SortedArraySet[A](map.updated(elem, null))

  def rangeImpl(from: Option[A], until: Option[A]) =
    new SortedArraySet[A](map.rangeImpl(from, until))

  def iteratorFrom(start: A) = map.keysIteratorFrom(start)
}

object SortedArraySet extends SortedIterableFactory[SortedArraySet] {
  def empty[A: Ordering]: SortedArraySet[A] = new SortedArraySet[A]

  def from[A](
      it: IterableOnce[A]
  )(implicit ordering: Ordering[A]): SortedArraySet[A] = {
    new SortedArraySet[A](SortedArrayMap.from(it.iterator.map((_, null))))
  }

  def newBuilder[A](implicit
      ordering: Ordering[A]
  ): ReusableBuilder[A, SortedArraySet[A]] =
    new SortedArraySetBuilder[A]

  private class SortedArraySetBuilder[A](implicit ordering: Ordering[A])
      extends ReusableBuilder[A, SortedArraySet[A]] {
    private var vals: ArrayBuffer[A] = ArrayBuffer()

    def addOne(elem: A): this.type = {
      vals.addOne(elem)
      this
    }

    override def addAll(xs: IterableOnce[A]): this.type = {
      vals.addAll(xs)
      this
    }

    override def clear(): Unit = {
      vals.clear()
    }

    override def result(): SortedArraySet[A] =
      new SortedArraySet(
        SortedArrayMap.from(vals.sortInPlace().iterator.map((_, null)))
      )
  }
}
