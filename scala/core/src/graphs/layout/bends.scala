package semagrams.graphs

import semagrams._
import semagrams.acsets._
import semagrams.rendering._













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
def makeBends[D:PartData](
  edges: Seq[EdgeSource[D]], 
  spacing: Double
)(a: ACSet[D]): Map[EntityTag,Double] =
  
  val es = edges.flatMap(_.tagSpans(a)).collect{
    case (e,(src,Some(tgt))) => e -> (src,tgt)
  }

  def ordered(v1:EntityTag,v2:EntityTag): (EntityTag,EntityTag) =
    if v1 <= v2 then (v1,v2) else (v2,v1)

  val emap = es.groupBy{ case apex -> (src,tgt) => ordered(src,tgt)}

  emap.flatMap{ case (src,tgt) -> someEs =>
    someEs.zipWithIndex.map{ case ((tag,_),idx) =>
      tag -> FixedSpacing(spacing,true).assignPos(idx,someEs.length)
    }
  }

  
def addBends[D:PartData](emap: EntitySeq[D],bends:Map[EntityTag,Double]): EntitySeq[D] =
  emap.map{ case (tag,(spr,init)) => 
    if bends.contains(tag)
    then tag -> (spr,init.setProp(Bend,bends(tag)))
    else tag -> (spr,init)
  }