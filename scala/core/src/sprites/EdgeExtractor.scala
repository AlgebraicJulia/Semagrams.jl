package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

def edgeProps(
    sprites: Sprites,
    srcEnt: Option[Entity],
    srcCenter: Complex,
    tgtEnt: Option[Entity],
    tgtCenter: Complex,
    bend: Double,
): PropMap = {
  val dir = srcCenter - tgtCenter
  val rot = Complex(0, bend).exp
  val start = srcEnt
    .flatMap(sprites.boundaryPt(_, -dir * rot))
    .getOrElse(srcCenter)
  val nd = tgtEnt
    .flatMap(sprites.boundaryPt(_, dir * rot.cong))
    .getOrElse(tgtCenter)
  PropMap() + (Start, start) + (End, nd) + (Bend, bend)
}

def edgeExtractor[S: IsSchema](
    ob: Ob,
    src: Hom,
    tgt: Hom
)(acs: ACSet[S], sprites: Sprites, bends: Map[Entity, Double]) = {
  def getProps(e: Part, bend: Double): PropMap = {
    val srcEnt = acs.trySubpart(src, e)
    val tgtEnt = acs.trySubpart(tgt, e)
    val srcCenter =
      srcEnt.map(sprites(_)._2(Center)).getOrElse(acs.subpart(Start, e))
    val tgtCenter =
      tgtEnt.map(sprites(_)._2(Center)).getOrElse(acs.subpart(End, e))
    acs.props(e) ++ edgeProps(sprites, srcEnt, srcCenter, tgtEnt, tgtCenter, bend)
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
        .map(e => (e, acs.trySubpart(src, e), acs.trySubpart(tgt, e), mul))
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
