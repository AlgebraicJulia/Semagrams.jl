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
    given petriACSet: ACSet[Petri] with
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

    def apply() = petriACSet.empty
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
    given labelledPetriACSet: ACSet[LabelledPetri] with
      val bare = GenIso[LabelledPetri, BareACSet]
      val schema = Schema(
        S,
        T,
        I,
        O,
        IT,
        IS,
        OT,
        OS,
        SName,
        TName
      )

    def apply() = labelledPetriACSet.empty
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
    given labelledReactionNetACSet: ACSet[LabelledReactionNet] with
      val bare = GenIso[LabelledReactionNet, BareACSet]
      val schema = Schema(
        S,
        T,
        I,
        O,
        IT,
        IS,
        OT,
        OS,
        SName,
        TName,
        Rate,
        Concentration
      )

    def apply() = labelledReactionNetACSet.empty
  }
}
