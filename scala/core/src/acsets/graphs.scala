package semagrams.acsets

object Graphs {
  case object V extends Ob
  case object E extends Ob

  case object Src extends Hom {
    val doms = E.asDom()
    val codoms = V.asDom()
  }
  case object Tgt extends Hom {
    val doms = E.asDom()
    val codoms = V.asDom()
  }

  case object SchGraph extends Schema {
    val obs = Seq(V,E)
    val homs = Seq(Src,Tgt)
    val attrs = Seq()
  }

  object Graph {
    def apply() = ACSet(SchGraph)
  }
}
