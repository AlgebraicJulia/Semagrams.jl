package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

def edgeExtractor[X <: Ob, Y <: Ob, Z <: Ob, A: ACSet](
    src: Hom[X, Y],
    tgt: Hom[X, Z]
)(acs: WithProps[A], sprites: Sprites, bends: Map[Entity, Double])(implicit
    withPropsACSet: ACSet[WithProps[A]]
) = {
  def getProps(e: Elt[X], bend: Double): PropMap = {
    val srcEnt = acs.subpart(src, e)
    val tgtEnt = acs.subpart(tgt, e)
    val p = acs.subpart(Props(src.dom.asInstanceOf[X]), e).get
    val srcCenter =
      srcEnt.map(sprites(_)._2(Center)).getOrElse(p(Start))
    val tgtCenter =
      tgtEnt.map(sprites(_)._2(Center)).getOrElse(p(End))
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
    acs
      .subpart(Props(src.dom.asInstanceOf[X]), e)
      .get + (Start, start) + (End, nd) + (Bend, bend)
  }

  acs
    .parts(src.dom.asInstanceOf[X])
    .map(e => (e, getProps(e, bends(e))))
    .toList
}

def assignBends[A: ACSet](
    edges: List[(Ob, AbstractHom, AbstractHom, Int)],
    acs: WithProps[A],
    increment: Double
)(implicit
    withPropsACSet: ACSet[WithProps[A]]
): Map[Entity, Double] = {
  edges
    .map({ case (ob, src, tgt, mul) =>
      acs
        .parts(ob)
        .map(v =>
          (v, acs.untypedSubpart(src, v), acs.untypedSubpart(tgt, v), mul)
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
