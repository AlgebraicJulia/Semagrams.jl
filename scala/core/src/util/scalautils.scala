package semagrams.util

import scala.annotation.targetName



extension [K,V](pairs:Iterable[(K,V)])
  def mapKeys[A](f:K => A,merge:(V,V) => V = (_:V,v:V) => v) = 
    val grps = pairs.groupBy{ case (k,v) => f(k) }

    for 
      a <- pairs.map{ case (k:K,v:V) => f(k) }
    yield 
      a -> grps(a).map(_._2).reduce(merge)
    

  def mapVals[A](f:V => A) =
    pairs.map((k,v) => k -> f(v))



extension [K,V](map:Map[K,V])


  def merge(mergeOp:(V,V) => V = (_:V,v:V) => v)(others:Iterable[(K,V)]): Map[K,V] =
    (map.toSeq ++ others).groupBy(_._1).map{ (k,kvs) =>
      kvs.reduce{ case ((k,v1),(_,v2)) => k -> mergeOp(v1,v2) }
    }
    
