package semagrams.acsets

import semagrams._

object Petris {
  case object S extends Ob
  case object T extends Ob
  case object I extends Ob
  case object O extends Ob

  case object IT extends HomWithDom {
    val dom = I
    val codom = T
  }
  case object IS extends HomWithDom {
    val dom = I
    val codom = S
  }
  case object OT extends HomWithDom {
    val dom = O
    val codom = T
  }
  case object OS extends HomWithDom {
    val dom = O
    val codom = S
  }

  case object SchPetri extends StaticSchema {
    val schema = BasicSchema(
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

  type Petri = ACSet[SchPetri.type]

  object Petri {
    def apply() = ACSet[SchPetri.type]()
  }

  case object TName extends AttrWithDom with PValue[String] {
    val dom = T
  }

  case object SName extends AttrWithDom with PValue[String] {
    val dom = S
  }

  case object SchLabelledPetri extends StaticSchema {
    val schema = SchPetri.extend(TName, SName)
  }

  type LabelledPetri = ACSet[SchLabelledPetri.type]

  object LabelledPetri {
    def apply() = ACSet[SchLabelledPetri.type]()
  }

  case object Rate extends AttrWithDom with PValue[Double] {
    val dom = T
  }

  case object Concentration extends AttrWithDom with PValue[Double] {
    val dom = S
  }

  case object SchLabelledReactionNet extends StaticSchema {
    val schema = SchLabelledPetri.extend(Concentration, Rate)
  }

  type LabelledReactionNet = ACSet[SchLabelledReactionNet.type]

  object LabelledReactionNet {
    def apply() = ACSet[SchLabelledReactionNet.type]()
  }
}
