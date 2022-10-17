package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

def edgeExtractor[S: IsSchema](
    ob: Ob,
    src: Hom,
    tgt: Hom
)(acs: ACSet[S], sprites: Sprites, bends: Map[Entity, Double]) = {
  def getProps(e: Entity, bend: Double): PropMap = {
    val srcEnt = acs.trySubpart(src, e)
    val tgtEnt = acs.trySubpart(tgt, e)
    val srcCenter =
      srcEnt.map(sprites(_)._2(Center)).getOrElse(acs.subpart(Start, e))
    val tgtCenter =
      tgtEnt.map(sprites(_)._2(Center)).getOrElse(acs.subpart(End, e))
    val dir = srcCenter - tgtCenter
    val rot = Complex(0, bend).exp
    val start = srcEnt
      .map(ent => {
        val (s, p) = sprites(ent)
        s.boundaryPt(ent, p, -dir * rot)
      })
      .getOrElse(srcCenter)
    val nd = tgtEnt
      .map(ent => {
        val (s, p) = sprites(ent)
        s.boundaryPt(ent, p, dir * rot.cong)
      })
      .getOrElse(tgtCenter)
    acs.props(e) + (Start, start) + (End, nd) + (Bend, bend)
  }

  acs
    .parts(ob)
    .map(e => (e, getProps(e, bends(e))))
    .toList
}

def assignBends[S: IsSchema](
    edges: List[(Ob, Hom, Hom, Int)],
    acs: ACSet[S],
    increment: Double
): Map[Entity, Double] = {
  edges
    .map({ case (ob, src, tgt, mul) =>
      acs
        .parts(ob)
        .map(e =>
          (e, acs.trySubpart(src, e), acs.trySubpart(tgt, e), mul)
        )
        .toList
    })
    .flatten
    .assignBends({ case (v, s, t, mul) => (s, t) }, increment)
    .map({ case ((v, s, t, mul), bend) => (v, mul * bend) })
    .toMap
}

extension [A](xs: List[A]) {
  def assignBends[B](f: A => B, increment: Double) = xs
    .groupBy(f)
    .values
    .map(l => {
      val n: Double = l.length
      l.zipWithIndex.map({ case (a, k) =>
        (a, ((-n + 1) / 2.0 + k) * increment)
      })
    })
    .toList
    .flatten
}
