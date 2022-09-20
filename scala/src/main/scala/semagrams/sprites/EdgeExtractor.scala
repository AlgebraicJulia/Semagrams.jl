package semagrams.sprites

import semagrams.acsets._

def edgeExtractor[X <: Ob, Y <: Ob, Z <: Ob, A: ACSet](
    src: Hom[X, Y],
    tgt: Hom[X, Z]
)(acs: A, sprites: Sprites) = {
  def getProps(e: Elt[X]): PropMap = {
    val srcEnt = acs.subpart(src, e).get
    val tgtEnt = acs.subpart(tgt, e).get
    val (sSprite, sPropMap) = sprites(srcEnt)
    val (tSprite, tPropMap) = sprites(tgtEnt)
    val dir = tPropMap(Center) - sPropMap(Center)
    val start = sSprite.boundaryPt(srcEnt, sPropMap, dir)
    val nd = tSprite.boundaryPt(tgtEnt, tPropMap, -dir)
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
