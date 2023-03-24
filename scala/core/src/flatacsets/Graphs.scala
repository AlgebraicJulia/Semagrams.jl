package semagrams.flatacsets

import semagrams._
import cats.data.State
import upickle.default._

object Graphs {
  case object V extends Ob
  case object E extends Ob

  case object Src extends HomWithDom {
    val dom = E
    val codom = V
  }

  case object Tgt extends HomWithDom {
    val dom = E
    val codom = V
  }

  case object SchGraph extends StaticSchema {
    val schema = BasicSchema(V, E, Src, Tgt)
  }

  type Graph = ACSet[SchGraph.type]

  trait HasGraph[S: IsSchema]

  trait GraphOps[S: IsSchema] extends ACSetOps[S] {
    extension (a: ACSet[S]) {
      def vertices() = a.parts(V)
      def edges() = a.parts(E)

      def src(e: Part) = a.subpart(Src, e)
      def tgt(e: Part) = a.subpart(Tgt, e)
    }

    def addVertex: State[ACSet[S], Part] = addVertex(PropMap())

    def addVertex(props: PropMap): State[ACSet[S], Part] = addPart(V, props)

    def addEdge(s: Part, t: Part, props: PropMap): State[ACSet[S], Part] = for {
      e <- addPart(E, props)
      _ <- setSubpart(Src, e, s)
      _ <- setSubpart(Tgt, e, t)
    } yield e

    def addEdge(s: Part, t: Part): State[ACSet[S], Part] =
      addEdge(s, t, PropMap())
  }

  implicit val graphOps: GraphOps[SchGraph.type] =
    new GraphOps[SchGraph.type] {}

  object Graph {
    def apply() = ACSet[SchGraph.type]()

    export graphOps._
  }

  case class Weight[T: ReadWriter]() extends AttrWithDom {
    val dom = E
    type Value = T
    val rw = summon[ReadWriter[T]]
  }

  // case class SchWeightedGraph[T: ReadWriter]() extends StaticSchema {
  //   val schema = SchGraph.extend(Weight[T]())
  // }

  // given[T: ReadWriter]: Pointed[SchWeightedGraph[T]] = new Pointed[SchWeightedGraph[T]] {
  //   def value: SchWeightedGraph[T] = SchWeightedGraph[T]()
  // }

  // type WeightedGraph[T] = ACSet[SchWeightedGraph[T]]

  // object WeightedGraph {
  //   def apply[T: ReadWriter]() = ACSet[SchWeightedGraph[T]]()

  //   def apply[T: ReadWriter] = new GraphOps[SchWeightedGraph[T]] {}
  // }
}
