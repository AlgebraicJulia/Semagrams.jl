package semagrams.rendering

import semagrams._
import semagrams.util._
import semagrams.partprops._

type EntitySeq[D] = Seq[(PartTag, (Sprite[D], D))]

object EntitySeq:
  def apply[D: PartData](): EntitySeq[D] = Seq[(PartTag, (Sprite[D], D))]()

object EntitySeqExtension {
  extension [D: PartData](entseq: EntitySeq[D])
    def tags = entseq.map(_._1)

    /** Find the center of the sprite corresponding to `p`, by looking up the
      * sprite/data in `entseq`
      */
    def findCenter(p: PartTag) =
      entseq
        .filter(_._1 == p)
        .flatMap { case (tag, (spr, init)) => spr.center(init, Seq()) }
        .headOption

    /** Find the center of the sprite corresponding to `p`, by looking up the
      * sprite/data in `entseq`
      */
    def findCenter(p: PartTag, subparts: Seq[PartTag]) =
      entseq
        .filter(_._1 == p)
        .flatMap { case (tag, (spr, init)) => spr.center(init, subparts) }
        .headOption

    /** Find the point on the boundary in direction `dir` of the sprite
      * corresponding to `p`, by looking up the sprite/data in `m`
      */
    def findBoundary(p: PartTag, dir: Complex) =
      entseq
        .filter(_._1 == p)
        .flatMap { case (tag, (spr, init)) => spr.boundaryPt(init, dir, Seq()) }
        .headOption

    def findBoundary(p: PartTag, dir: Complex, subparts: Seq[PartTag]) =
      entseq
        .filter(_._1 == p)
        .flatMap { case (tag, (spr, init)) =>
          spr.boundaryPt(init, dir, subparts)
        }
        .headOption

    def getTag(p: PartTag): Option[(Sprite[D], D)] =
      entseq.find(_._1 == p).map(_._2)
}
export EntitySeqExtension._
