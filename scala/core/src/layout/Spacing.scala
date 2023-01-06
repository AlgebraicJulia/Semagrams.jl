package semagrams.layout

import semagrams._
import semagrams.acsets._

trait Spacer {
  def assignPos(i: Int, n: Int): Double

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

  def assignPositions[S: IsSchema](ob: Ob, by: Property, p: Property { type Value = Double })(
    a: ACSet[S]
  ): ACSet[S] = {
    val positions = spaceBy(a.trySubpart(by, _))(a.parts(ob))
    positions.foldLeft(a)({ case (b, (e, pos)) => b.setSubpart(p, e, pos) })
  }
}

case class FixedRange(from: Double, to: Double) extends Spacer {
  def assignPos(i: Int, n: Int) =
    from + (i.toFloat / (n.toFloat - 1)) * (to - from)
}

case class FixedRangeExceptEnds(from: Double, to: Double) extends Spacer {
  def assignPos(i: Int, n: Int) = FixedRange(from, to).assignPos(i+1, n+2)

}

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

def assignBends[S: IsSchema](edges: Map[Ob, (Hom, Hom)], spacing: Double)(
    a: ACSet[S]
): ACSet[S] = {
  def ends(i: Part) = {
    val (src, tgt) = edges(i.ob)
    for {
      s <- a.trySubpart(src, i)
      t <- a.trySubpart(tgt, i)
    } yield (s, t)
  }

  def sortedEnds(i: Part) =
    ends(i).map({
      case (s, t) if s.id <= t.id => (s, t)
      case (s, t)                 => (t, s)
    })

  val parts = edges.keys.map(a.parts(_).toSeq).foldLeft(Seq[Part]())(_ ++ _)
  val bends = FixedSpacing(spacing, true)
    .spaceBy(sortedEnds)(parts)
    .map((i, bend) =>
      ends(i) match {
        case Some((s, t)) if s.id > t.id => (i, -bend)
        case _                           => (i, bend)
      }
    )

  bends.foldLeft(a)({ case (b, (i, bend)) => b.setSubpart(Bend, i, bend) })
}

