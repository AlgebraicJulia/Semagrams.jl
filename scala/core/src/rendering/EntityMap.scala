package semagrams.rendering


import semagrams._
import semagrams.util._





type EntitySeq[D] = Seq[(EntityTag,(Sprite[D],D))]

object EntitySeq:
  def apply[D:PartData](): EntitySeq[D] = Seq[(EntityTag,(Sprite[D],D))]()




object EntitySeqExtension {
  extension [D:PartData](entseq:EntitySeq[D])
    def tags = entseq.map(_._1)
    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `entseq`
    */
    def findCenter(p:Part) =
      entseq.filter((tag,_) => tag.keyPart == p)
        .flatMap{ case (tag,(spr,init)) => spr.center(init,Seq()) }
        .headOption
    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `entseq`
    */
    def findCenter(p:Part,subparts:Seq[Part]) =
      entseq.filter((tag,_) => tag.keyPart == p)
        .flatMap{ case (tag,(spr,init)) => spr.center(init,subparts) }
        .headOption


    /** Find the point on the boundary in direction `dir` of the sprite
      * corresponding to `p`, by looking up the sprite/data in `m`
      */
    def findBoundary(p: Part,dir: Complex) = 
      entseq.filter((tag,_) => tag.keyPart == p)
        .flatMap{ case (tag,(spr,init)) => spr.boundaryPt(init,dir,Seq()) }
        .headOption
    
    def findBoundary(p: Part,dir: Complex,subparts:Seq[Part]) = 
      entseq.filter((tag,_) => tag.keyPart == p)
        .flatMap{ case (tag,(spr,init)) => spr.boundaryPt(init,dir,subparts) }
        .headOption
    
    def getPart(p:Part,ctxt:UID): Option[(Sprite[D],D)] =
      entseq.find((tag,_) => tag.contextId == ctxt & tag.keyPart == p)
        .map(_._2)
}
export EntitySeqExtension._

