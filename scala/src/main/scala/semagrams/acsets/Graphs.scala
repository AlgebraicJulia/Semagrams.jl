package semagrams.acsets

import monocle.macros.GenIso
import cats.data.State

case object E extends Ob

case object V extends Ob

case object Src extends Hom[E.type, V.type] {
  val dom = E
  val codom = V
}

case object Tgt extends Hom[E.type, V.type] {
  val dom = E
  val codom = V
}

case class WeightValue[T]() extends AttrType {
  type Value = T
}

case class Weight[T]() extends Attr[E.type, T] {
  val dom = E
  val codom = WeightValue[T]()
}

trait HasGraph[A: ACSet] {
  extension (a: A)
    def vertices(): Set[Elt[V.type]] = a.parts(V)
    def edges(): Set[Elt[E.type]] = a.parts(E)

    def src(e: Elt[E.type]): Option[Elt[V.type]] = a.subpart(Src, e)
    def tgt(e: Elt[E.type]): Option[Elt[V.type]] = a.subpart(Tgt, e)
}

def addVertex[A: HasGraph: ACSet](): State[A, Elt[V.type]] = addPart(V)

def addEdge[A: HasGraph: ACSet](
    s: Elt[V.type],
    t: Elt[V.type]
): State[A, Elt[E.type]] =
  for {
    e <- addPart(E)
    _ <- setSubpart(Src, e, s)
    _ <- setSubpart(Tgt, e, t)
  } yield e

case class Graph(acset: BareACSet)

given graphACSet: ACSet[Graph] with
  val bare = GenIso[Graph, BareACSet]
  val schema = Schema(
    E,
    V,
    Src,
    Tgt
  )

object Graph {
  def apply() = graphACSet.empty
}

given graphHasGraph: HasGraph[Graph] = new HasGraph {}

case class WeightedGraph[T](acset: BareACSet)

given weightedGraphACSet[T]: ACSet[WeightedGraph[T]] with
  val bare = GenIso[WeightedGraph[T], BareACSet]
  val schema = Schema(
    E,
    V,
    Src,
    Tgt,
    Weight[T]()
  )

object WeightedGraph {
  def apply[T]() = weightedGraphACSet[T].empty
}

given weightedGraphHasGraph[T]: HasGraph[WeightedGraph[T]] = new HasGraph {}

case class LabelValue[T]() extends AttrType {
  type Value = T
}

case class Label[T]() extends Attr[V.type, T] {
  val dom = V
  val codom = LabelValue[T]()
}

case class LabeledGraph[T](acset: BareACSet)

given labeledGraphACSet[T]: ACSet[LabeledGraph[T]] with
  val bare = GenIso[LabeledGraph[T], BareACSet]
  val schema = Schema(
    E,
    V,
    Src,
    Tgt,
    Label[T]()
  )

object LabeledGraph {
  def apply[T]() = labeledGraphACSet[T].empty
}

given labeledGraphHasGraph[T]: HasGraph[LabeledGraph[T]] = new HasGraph {}

def addLabeledVertex[T](label: T): State[LabeledGraph[T], Elt[V.type]] =
  for {
    v <- addVertex()
    _ <- setSubpart(Label[T](), v, label)
  } yield v
