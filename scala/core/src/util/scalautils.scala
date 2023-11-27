package semagrams.util




extension [K,V](pairs:Iterable[(K,V)])
  def mapKeys[A](f:K => A,merge:(V,V) => V = (_:V,v:V) => v) = 
    val grps = pairs.toSeq.groupBy((pair:(K,V)) => f(pair._1) )

    for 
      a <- pairs.map{ case (k:K,v:V) => f(k) }
    yield 
      a -> grps(a).map(_._2).reduce(merge)
    

  def mapVals[A](f:V => A) =
    pairs.map((k,v) => k -> f(v))



extension [K,V](map:Map[K,V])


  def merge(mergeOp:(V,V) => V = (_:V,v:V) => v)(others:Iterable[(K,V)]): Map[K,V] =
    (map.toSeq ++ others).groupBy(_._1).map{ (_,kvs) =>
      kvs.reduce{ case ((k,v1),(_,v2)) => k -> mergeOp(v1,v2) }
    }
    
