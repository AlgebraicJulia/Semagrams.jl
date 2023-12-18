package semagrams.graphs

import semagrams._

/** An implementation of this trait describes how to assign real numbers to
  * elements of a finite set {0,...,n-1}.
  */
trait Spacer {

  /** Positions the ith element of {0,...,n-1} */
  def assignPos(i: Int, n: Int): Double

  /** Returns a map of element => position, where each element is assigned a
    * position based on its index when `xs` is grouped by `f`.
    *
    * Elements for which `f` returns `None` are not included in the map.
    *
    * @param f
    *   the function to group `xs` by
    *
    * @param `xs`
    *   the list of things to assign positions to
    */
  def spaceBy[X, T](f: X => Option[T])(xs: Seq[X]): Map[X, Double] =
    xs.collect(((x: X) => f(x).map((x, _))).unlift)
      .groupBy(_._2)
      .map((_, g) =>
        g.map(_._1)
          .zipWithIndex
          .map((x, i) => (x, assignPos(i, g.length)))
          .toMap
      )
      .foldLeft(Map[X, Double]())(_ ++ _)
}

/** An implementation of [[Spacer]] that assigns `from` to `0` and `to` to
  * `n-1`, with the rest of the elements evenly spaced between them.
  */
case class FixedRange(from: Double, to: Double) extends Spacer {
  def assignPos(i: Int, n: Int) =
    from + (i.toFloat / (n.toFloat - 1)) * (to - from)
}

/** An implementation of [[Spacer]] that assigns `from` to `-1` and `to` to `n`,
  * with the rest of the elements evenly space between them.
  *
  * Useful when you want to, for instance, put ports evenly on a side of a box
  * but you don't want to put ports on the corners of the box.
  */
case class FixedRangeExceptEnds(from: Double, to: Double) extends Spacer {
  def assignPos(i: Int, n: Int) = FixedRange(from, to).assignPos(i + 1, n + 2)

}

/** An implementation of [[Spacer]] that spaces the elements according to a
  * fixed spacing. If `centered` is true, then the elements are spaced centered
  * around 0, otherwise the elements are spaced starting from 0.
  *
  * Useful for assigning bends to the edges going between two vertices.
  */
case class FixedSpacing(spacing: Double, centered: Boolean) extends Spacer {
  def assignPos(i: Int, n: Int) = {
    val offset = if (centered) {
      (n.toDouble - 1) / 2
    } else {
      0.0
    }
    (i - offset) * spacing
  }
}
