package semagrams.flatacsets

object DirectedPortGraph {
  case object Box extends Ob
  case object InPort extends Ob
  case object OutPort extends Ob
  case object Wire extends Ob

  case object InParent extends HomWithDom {
    val dom = InPort
    val codom = Box
  }

  case object OutParent extends HomWithDom {
    val dom = OutPort
    val codom = Box
  }

  case object Src extends HomWithDom {
    val dom = Wire
    val codom = OutPort
  }

  case object Tgt extends HomWithDom {
    val dom = Wire
    val codom = InPort
  }

  case object SchDirectedPortGraph extends StaticSchema {
    val schema = BasicSchema(
      Box,
      InPort,
      OutPort,
      Wire,
      InParent,
      OutParent,
      Src,
      Tgt
    )
  }

  type DirectedPortGraph = ACSet[SchDirectedPortGraph.type]

  object DirectedPortGraph {
    def apply() = ACSet[SchDirectedPortGraph.type]()
  }
}
