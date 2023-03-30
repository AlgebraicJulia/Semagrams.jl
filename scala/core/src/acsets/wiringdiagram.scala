package semagrams.acsets

import semagrams._

/** Implementation of a schema for wiring diagrams
  *
  * Uses nested acsets so that ports are parts of the subacsets corresponding to
  * boxes.
  */
object WiringDiagrams {
  case object Box extends Ob {
    override val schema = SchWiringDiagram
  }

  case object PrimBox extends Ob {
    override val schema = SchPrimBox
  }

  case object Program extends Attr with PValue[String] {
    val dom = Seq(ROOT.ty)
  }

  case object OutPort extends Ob

  case object InPort extends Ob

  case object SchPrimBox extends Schema {
    val obs = Seq(OutPort, InPort)
    val homs = Seq()
    val attrs = Seq(Program)
  }

  case object Wire extends Ob

  case object Src extends Hom {
    val doms = Seq(PartType(Seq(Wire)))
    val codoms = Seq(
      PartType(Seq(Box, OutPort)),
      PartType(Seq(PrimBox, OutPort)),
      PartType(Seq(InPort))
    )
  }

  case object Tgt extends Hom {
    val doms = Seq(PartType(Seq(Wire)))
    val codoms = Seq(
      PartType(Seq(Box, InPort)),
      PartType(Seq(PrimBox, InPort)),
      PartType(Seq(OutPort))
    )
  }

  case object SchWiringDiagram extends Schema {
    val obs = Seq(Box, OutPort, InPort, Wire)
    val homs = Seq(Src, Tgt)
    val attrs = Seq()
  }

  object WiringDiagram {
    def apply() = ACSet(SchWiringDiagram)
  }
}
