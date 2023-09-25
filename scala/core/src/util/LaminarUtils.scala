package semagrams.util

import com.raquo.laminar.api.L

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))


def printObs(s:String) = L.Observer(_ => println(s))
