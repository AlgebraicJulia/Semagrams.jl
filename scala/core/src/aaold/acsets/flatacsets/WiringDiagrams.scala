// package semagrams.flatacsets

// import semagrams._
// import upickle.default._

// object WiringDiagrams {
//   case object Box extends Ob
//   case object Wire extends Ob

//   sealed trait SrcPort extends Entity

//   object SrcPort extends EntityType {
//     val id = util.UID("SrcPort")
//     case class Box(box: Int, slot: Int) extends SrcPort {
//       val id = util.UID("Box")
      
//       val ty = SrcPort
//     }
//     case class Diagram(slot: Int) extends SrcPort {
//       val id = util.UID("DIagram")
    
//       val ty = SrcPort
//     }

//     object Box {
//       implicit val rw: ReadWriter[Box] = macroRW
//     }

//     object Diagram {
//       implicit val rw: ReadWriter[Diagram] = macroRW
//     }

//     implicit val rw: ReadWriter[SrcPort] = ReadWriter.merge(Box.rw, Diagram.rw)
//   }

//   sealed trait TgtPort extends Entity

//   object TgtPort extends EntityType {
//     case class Box(box: Int, slot: Int) extends TgtPort {
//       val id = util.UID("Box")
    
//       val ty = TgtPort
//     }
//     case class Diagram(slot: Int) extends TgtPort {
//       val id = util.UID("Diagram")
    
//       val ty = TgtPort
//     }

//     object Box {
//       implicit val rw: ReadWriter[Box] = macroRW
//     }

//     object Diagram {
//       implicit val rw: ReadWriter[Diagram] = macroRW
//     }

//     implicit val rw: ReadWriter[TgtPort] = ReadWriter.merge(Box.rw, Diagram.rw)
//   }

//   case object Src extends AttrWithDom with PValue[SrcPort] {
//     val dom = Wire
//   }

//   case object Tgt extends AttrWithDom with PValue[TgtPort] {
//     val dom = Wire
//   }

//   case class BoxType[P](inports: Seq[P], outports: Seq[P])

//   implicit def boxTypeRW[P: ReadWriter]: ReadWriter[BoxType[P]] = macroRW

//   case object boxTy extends AttrWithDom with PValue[BoxType[Unit]] {
//     val dom = Box
//   }

//   case class diagTy[P: ReadWriter]() extends Global with PValue[BoxType[P]]

//   case object SchWiringDiagram extends StaticSchema {
//     val schema = BasicSchema(
//       Box,
//       boxTy,
//       Wire,
//       Src,
//       Tgt,
//       diagTy[Unit]()
//     )
//   }

//   type WiringDiagram = ACSet[SchWiringDiagram.type]

//   object WiringDiagram {
//     def apply() = ACSet[SchWiringDiagram.type]()

//   }
// }
