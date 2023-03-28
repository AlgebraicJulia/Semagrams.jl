package semagrams.layout

import semagrams._
import semagrams.acsets._

/** Evenly space the edges in an acset by assigning bends to them
  *
  * @param edges
  *   a collection of objects to assign bends to, along with a source and target
  *   for each object. We want multiple objects because there could be different
  *   types of edge that all need to be spaced together.
  *
  * @param spacing
  *   the increment between bends of adjacent edges
  *
  * @param a
  *   the acset to assign bends to.
  */
def assignBends(edges: Map[Ob, (Hom, Hom)], spacing: Double)(
    a: ACSet
): ACSet = {
  def ends(i: Part) = {
    val (src, tgt) = edges(i.ty.path(0))
    for {
      s <- a.trySubpart(src, i)
      t <- a.trySubpart(tgt, i)
    } yield (s, t)
  }

  def sortedEnds(i: Part) =
    ends(i).map({
      case (s, t) if s.hashCode() <= t.hashCode() => (s, t)
      case (s, t)                                 => (t, s)
    })

  val parts =
    edges.keys.map(a.parts(ROOT, _).map(_._1)).foldLeft(Seq[Part]())(_ ++ _)
  val bends = FixedSpacing(spacing, true)
    .spaceBy(sortedEnds)(parts)
    .map((i, bend) =>
      ends(i) match {
        case Some((s, t)) if s.hashCode() > t.hashCode() => (i, -bend)
        case _                                           => (i, bend)
      }
    )

  bends.foldLeft(a)({ case (b, (i, bend)) => b.setSubpart(i, Bend, bend) })
}
