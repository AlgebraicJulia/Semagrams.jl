package semagrams.acsets

object WiringDiagrams {
  case object Box extends Ob {
    override val schema = SchWiringDiagram
  }

  case object OutPort extends Ob

  case object InPort extends Ob

  case object Wire extends Ob

  case object Src extends Hom {
    val dom = PartType(Seq(Wire))
    val codom = PartType(Seq(Box, OutPort))
  }

  case object Tgt extends Hom {
    val dom = PartType(Seq(Wire))
    val codom = PartType(Seq(Box, InPort))
  }

  case object InWire extends Ob

  case object InSrc extends Hom {
    val dom = PartType(Seq(InWire))
    val codom = PartType(Seq(InPort))
  }

  case object InTgt extends Hom {
    val dom = PartType(Seq(InWire))
    val codom = PartType(Seq(Box, InPort))
  }

  case object OutWire extends Ob

  case object OutSrc extends Hom {
    val dom = PartType(Seq(OutWire))
    val codom = PartType(Seq(Box, OutPort))
  }

  case object OutTgt extends Hom {
    val dom = PartType(Seq(OutWire))
    val codom = PartType(Seq(OutPort))
  }

  case object ThroughWire extends Ob

  case object ThroughSrc extends Hom {
    val dom = PartType(Seq(ThroughWire))
    val codom = PartType(Seq(InPort))
  }

  case object ThroughTgt extends Hom {
    val dom = PartType(Seq(ThroughWire))
    val codom = PartType(Seq(OutPort))
  }

  case object SchWiringDiagram extends Schema {
    val obs = Seq(Box, OutPort, InPort, Wire, InWire, OutWire, ThroughWire)
    val homs = Seq(Src, Tgt, InSrc, InTgt, OutSrc, OutTgt, ThroughSrc, ThroughTgt)
    val attrs = Seq()
  }

  object WiringDiagram {
    def apply() = ACSet(SchWiringDiagram)
  }
}
