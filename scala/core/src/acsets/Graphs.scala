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

trait HasGraph[A] extends ACSetOps[A] {
  extension (a: A)
    def vertices(): Set[Elt[V.type]] = a.parts(V)
    def edges(): Set[Elt[E.type]] = a.parts(E)

    def src(e: Elt[E.type]): Option[Elt[V.type]] = a.subpart(Src, e)
    def tgt(e: Elt[E.type]): Option[Elt[V.type]] = a.subpart(Tgt, e)

  def addVertex(): State[A, Elt[V.type]] = addPart(V)

  def addEdge(
      s: Elt[V.type],
      t: Elt[V.type]
  ): State[A, Elt[E.type]] =
    for {
      e <- addPart(E)
      _ <- setSubpart(Src, e, s)
      _ <- setSubpart(Tgt, e, t)
    } yield e
}

given[A: HasGraph]: ACSet[A] = {
  val hasGraph = summon[HasGraph[A]]
  hasGraph.acsetInstance
}

case class Graph(acset: BareACSet)

object Graph {
  val ops = new HasGraph[Graph] {
    given acsetInstance: ACSet[Graph] with
      val bare = GenIso[Graph, BareACSet]
      val schema = Schema(
        E,
        V,
        Src,
        Tgt
      )
  }

  def apply() = ops.acsetInstance.empty
}

given HasGraph[Graph] = Graph.ops

case class WeightedGraph[T](acset: BareACSet)

object WeightedGraph {
  def ops[T] = new HasGraph[WeightedGraph[T]] {
    given acsetInstance: ACSet[WeightedGraph[T]] with
      val bare = GenIso[WeightedGraph[T], BareACSet]
      val schema = Schema(
        E,
        V,
        Src,
        Tgt,
        Weight[T]()
      )
  }

  def apply[T]() = ops[T].acsetInstance.empty
}

given[T]: HasGraph[WeightedGraph[T]] = WeightedGraph.ops[T]

case class LabelValue[T]() extends AttrType {
  type Value = T
}

case class Label[T]() extends Attr[V.type, T] {
  val dom = V
  val codom = LabelValue[T]()
}

case class LabeledGraph[T](acset: BareACSet)

object LabeledGraph {
  def ops[T] = new HasGraph[LabeledGraph[T]] {
    given acsetInstance: ACSet[LabeledGraph[T]] with
      val bare = GenIso[LabeledGraph[T], BareACSet]
      val schema = Schema(
        E,
        V,
        Src,
        Tgt,
        Label[T]()
      )
  }

  def apply[T]() = ops[T].acsetInstance.empty
}

given[T]: HasGraph[LabeledGraph[T]] = LabeledGraph.ops[T]

def addLabeledVertex[T](label: T): State[LabeledGraph[T], Elt[V.type]] = {
  val ops = LabeledGraph.ops[T]
  for {
    v <- ops.addVertex()
    _ <- ops.setSubpart(Label[T](), v, label)
  } yield v
}

type PropGraph = WithProps[Graph]

object PropGraphOps extends HasGraph[PropGraph] with PropOps[Graph] {
  val acsetInstance = WithProps.ops[Graph].acsetInstance
}

object PropGraph {
  def ops = PropGraphOps
}
