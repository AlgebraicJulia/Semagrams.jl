// package semagrams.acsets.nested

// import semagrams._

// /** Implementation of the schema for Petri nets */
// object Petris {
//   case object S extends NestedOb
//   case object T extends NestedOb
//   case object I extends NestedOb
//   case object O extends NestedOb

//   case object IT extends Hom {
//     val doms = I.asDom()
//     val codoms = T.asDom()
//   }
//   case object IS extends Hom {
//     val doms = I.asDom()
//     val codoms = S.asDom()
//   }
//   case object OT extends Hom {
//     val doms = O.asDom()
//     val codoms = T.asDom()
//   }
//   case object OS extends Hom {
//     val doms = O.asDom()
//     val codoms = S.asDom()
//   }

//   case object SName extends Attr with PValue[String] {
//     val doms = Seq(PartType(Seq(S)))
//   }

//   case object TName extends Attr with PValue[String] {
//     val doms = Seq(PartType(Seq(T)))
//   }

//   case object SchPetri extends Schema {
//     val obs = Seq(S, T, I, O)
//     val homs = Seq(IT, IS, OT, OS)
//     val attrs = Seq()
//   }

//   object Petri {
//     def apply() = ACSet(SchPetri)
//   }
// }
