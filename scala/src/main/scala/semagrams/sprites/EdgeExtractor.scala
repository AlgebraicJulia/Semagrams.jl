package semagrams.sprites

import semagrams.acsets._

def edgeExtractor[X <: Ob, Y <: Ob, Z <: Ob, A: ACSet](
    src: Hom[X, Y],
    tgt: Hom[X, Z]
)(acs: WithProps[A], sprites: Sprites)(implicit
    withPropsACSet: ACSet[WithProps[A]]
) = {
  def getProps(e: Elt[X]): PropMap = {
    val srcEnt = acs.subpart(src, e)
    val tgtEnt = acs.subpart(tgt, e)
    val srcCenter =
      srcEnt.map(sprites(_)._2(Center)).getOrElse(acs.getProp(e, Start))
    val tgtCenter =
      tgtEnt.map(sprites(_)._2(Center)).getOrElse(acs.getProp(e, End))
    val dir = srcCenter - tgtCenter
    val start = srcEnt
      .map(ent => {
        val (s, p) = sprites(ent)
        s.boundaryPt(ent, p, -dir)
      })
      .getOrElse(srcCenter)
    val nd = tgtEnt
      .map(ent => {
        val (s, p) = sprites(ent)
        s.boundaryPt(ent, p, dir)
      })
      .getOrElse(tgtCenter)
    PropMap() + (Start, start) + (End, nd)
  }

  acs
    .parts(src.dom)
    .toList
    .map(e => {
      val ep = e.asInstanceOf[Elt[X]]
      (ep, getProps(ep))
    })
}
