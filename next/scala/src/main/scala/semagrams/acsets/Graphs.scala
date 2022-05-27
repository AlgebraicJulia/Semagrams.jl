package semagrams.acsets

import monocle.macros.GenIso
import cats.data.State

case class E() extends Ob
case class V() extends Ob

case class Src() extends Hom[E, V]
case class Tgt() extends Hom[E, V]

case class Weight[T]() extends Attr[E, T]

trait HasGraph[A: ACSet] {
  extension(a: A)
    def vertices(): Set[Elt[V]] = a.parts(V())
    def edges(): Set[Elt[E]] = a.parts(E())

    def src(e: Elt[E]): Option[Elt[V]] = a.subpart(Src(), e)
    def tgt(e: Elt[E]): Option[Elt[V]] = a.subpart(Tgt(), e)
}

def addVertex[A: HasGraph: ACSet](): State[A, Elt[V]] = addPart(V())
def addEdge[A: HasGraph: ACSet](s: Elt[V], t: Elt[V]): State[A, Elt[E]] =
  for {
    e <- addPart(E())
    _ <- setSubpart(Src(), e, s)
    _ <- setSubpart(Tgt(), e, t)
  } yield e

case class Graph(acset: BareACSet)

given graphACSet: ACSet[Graph] with
  val bare = GenIso[Graph, BareACSet]
  val schema = Schema(
    E(), V(),
    Src(), Tgt()
  )

object Graph {
  def apply() = graphACSet.empty
}

given graphHasGraph: HasGraph[Graph] = new HasGraph {}

case class WeightedGraph[T](acset: BareACSet)

given weightedGraphACSet[T]: ACSet[WeightedGraph[T]] with
  val bare = GenIso[WeightedGraph[T], BareACSet]
  val schema = Schema(
    E(), V(),
    Src(), Tgt(),
    Weight[T]()
  )

object WeightedGraph {
  def apply[T]() = weightedGraphACSet[T].empty
}

given weightedGraphHasGraph[T]: HasGraph[WeightedGraph[T]] = new HasGraph {}
