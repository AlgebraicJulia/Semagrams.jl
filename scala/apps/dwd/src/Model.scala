package semagrams.dwd

import semagrams.api._
import semagrams.acsets._
import semagrams.PValue



case object WireType extends Ob {
  override val schema = SchType
}
case object WireTypeName extends Attr with PValue[String] {
  val dom = ROOT.ty
}
case object SchType extends Schema {
  val obs = Seq()
  val homs = Seq()
  val attrs = Seq(WireTypeName)
}


val testType = ACSet(SchType,PropMap().set(WireTypeName,"Person"))


// trait Port extends Ob {
//   override val schema = SchPort
// }
// case object InPort extends Port
// case object OutPort extends Port

// case object PortType extends Hom {
//   val doms = Seq(ROOT.ty)
//   val codoms = Seq(BaseType)
// }

// case object SchPort extends Schema {
//   val obs = Seq()
//   val homs = Seq(PortType)
//   val attrs
// }




// case object Boundary extends Ob {

//   override val schema = SchBoundary
// }

// case object BoundaryName extends Attr with PValue[String] {
//   val dom = ROOT.ty
// }



// case object InPort extends Ob {
//   override val schema = SchPort
// }
// case object OutPort extends Ob {
//   override val schema = SchPort
// }
// case object PortVar extends Attr with PValue[String] {
//   val dom = ROOT.ty
// }




// case object SchBoundary extends Schema {

//   val obs = Seq(InPort,OutPort)

//   val homs = Seq()

//   val attrs = Seq()
// }


// case object Layer extends Ob {
//   override val schema = LayerSchema
// }

// // uses default empty schema
// case object InteriorBoundary extends Ob

// case object LayerOfIntBound extends Hom {

//   val doms = Seq(PartType(Seq(InteriorBoundary)))

//   val codoms = Seq(PartType(Seq(Layer)))

// }

// case object BoundaryOfIntBound extends Hom {
//   val doms = Seq(PartType(Seq(InteriorBoundary)))
//   val codoms = Seq(PartType(Seq(Boundary)))
// }




// case object BaseType extends Ob