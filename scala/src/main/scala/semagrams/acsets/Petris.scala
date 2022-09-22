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

  given petriACSet: ACSet[Petri] with
    val bare = GenIso[Petri, BareACSet]
    val schema = Schema(
      S,T,I,O,
      IT,IS,OT,OS
    )

  object Petri {
    def apply() = petriACSet.empty
  }
}
