package semagrams.acsets

object WiringDiagrams {
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

  case object SchWiringDiagram extends StaticSchema {
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

  type WiringDiagram = ACSet[SchWiringDiagram.type]

  object WiringDiagram {
    def apply() = ACSet[SchWiringDiagram.type]()
  }
}
