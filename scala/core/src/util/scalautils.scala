package semagrams.util



extension [A,B](pairs:Iterable[(A,B)])
  def mapKeys[C](f:A => C,merge:(B,B) => B = (b1:B,b2:B) => b2) = 
    val grps = pairs.groupBy{ case (a,b) => f(a) }

    for 
      c <- pairs.map{ case (a:A,b:B) => f(a) }
    yield 
      c -> grps(c).map(_._2).reduce(merge)
    

  def mapVals[C](f:B => C) =
    pairs.map((a,b) => a -> f(b))
