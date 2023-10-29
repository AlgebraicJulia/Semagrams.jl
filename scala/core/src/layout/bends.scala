package semagrams.layout

import semagrams._
import semagrams.acsets.abstr._

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
def assignBends[A:ACSet](
  edges: Map[Ob, (PartProp, PartProp)], 
  spacing: Double
)(
  a: A
): A = {
  def ends(p: Part) = {
    val (src, tgt) = edges(p.ob)
    for {
      s <- a.tryProp(src, p)
      t <- a.tryProp(tgt, p)
    } yield (s, t)
  }

  def sortedEnds(p: Part) =
    ends(p).map({
      case (s, t) if s.hashCode() <= t.hashCode() => (s, t)
      case (s, t)                                 => (t, s)
    })

  val parts =
    edges.keys.map(a.getParts(_))
      .foldLeft(Seq[Part]())(_ ++ _)
  
  val bends = FixedSpacing(spacing, true)
    .spaceBy(sortedEnds)(parts)
    .map((p, bend) =>
      ends(p) match {
        case Some((s, t)) if s.hashCode() > t.hashCode() => (p, -bend)
        case _                                           => (p, bend)
      }
    )

  bends.foldLeft(a)({ case (b, (p, bend)) => b.setProp(Bend, p, bend) })
}
