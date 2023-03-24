package semagrams.acsets

import semagrams._

object Petris {
  case object S extends Ob
  case object T extends Ob
  case object I extends Ob
  case object O extends Ob

  case object IT extends Hom {
    val doms = I.asDom()
    val codoms = T.asDom()
  }
  case object IS extends Hom {
    val doms = I.asDom()
    val codoms = S.asDom()
  }
  case object OT extends Hom {
    val doms = O.asDom()
    val codoms = T.asDom()
  }
  case object OS extends Hom {
    val doms = O.asDom()
    val codoms = S.asDom()
  }

  case object SName extends Attr with PValue[String] {
    val dom = Seq(PartType(Seq(S)))
  }

  case object TName extends Attr with PValue[String] {
    val dom = Seq(PartType(Seq(T)))
  }

  case object SchPetri extends Schema {
    val obs = Seq(S, T, I, O)
    val homs = Seq(IT, IS, OT, OS)
    val attrs = Seq()
  }

  object Petri {
    def apply() = ACSet(SchPetri)
  }
}
