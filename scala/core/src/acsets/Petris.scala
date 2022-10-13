package semagrams.acsets

import monocle.macros.GenIso

object Petris {
  case object S extends Ob
  case object T extends Ob
  case object I extends Ob
  case object O extends Ob

  case object IT extends Hom[I.type, T.type] {
    val dom = I
    val codom = T
  }
  case object IS extends Hom[I.type, S.type] {
    val dom = I
    val codom = S
  }
  case object OT extends Hom[O.type, T.type] {
    val dom = O
    val codom = T
  }
  case object OS extends Hom[O.type, S.type] {
    val dom = O
    val codom = S
  }

  case class Petri(acset: BareACSet)

  object Petri {
    val ops = new ACSetOps[Petri] {
      given acsetInstance: ACSet[Petri] with
        val bare = GenIso[Petri, BareACSet]
        val schema = Schema(
          S,
          T,
          I,
          O,
          IT,
          IS,
          OT,
          OS
        )
    }

    def apply() = ops.empty
  }

  case object NameValue extends AttrType {
    type Value = String
  }

  case object TName extends Attr[T.type, String] {
    val dom = T
    val codom = NameValue
  }

  case object SName extends Attr[S.type, String] {
    val dom = S
    val codom = NameValue
  }

  case class LabelledPetri(acset: BareACSet)

  object LabelledPetri {
    val ops = new ACSetOps[LabelledPetri] {
      given acsetInstance: ACSet[LabelledPetri] with
        val bare = GenIso[LabelledPetri, BareACSet]
        val schema = Petri.ops.schema.extend(SName, TName)
    }

    def apply() = ops.empty
  }

  case object RateValue extends AttrType {
    type Value = Double
  }

  case object ConcentrationValue extends AttrType {
    type Value = Double
  }

  case object Rate extends Attr[T.type, Double] {
    val dom = T
    val codom = RateValue
  }

  case object Concentration extends Attr[S.type, Double] {
    val dom = S
    val codom = ConcentrationValue
  }

  case class LabelledReactionNet(acset: BareACSet)

  object LabelledReactionNet {
    val ops = new ACSetOps[LabelledReactionNet] {
      given acsetInstance: ACSet[LabelledReactionNet] with
        val bare = GenIso[LabelledReactionNet, BareACSet]
        val schema = LabelledPetri.ops.schema.extend(Rate, Concentration)
    }

    def apply() = ops.empty
  }

  given ACSet[LabelledReactionNet] = LabelledReactionNet.ops.acsetInstance
}
