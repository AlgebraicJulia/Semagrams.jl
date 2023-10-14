package acsets

// enum Data {
//   case Ref(tag: PatchTag)
//       case Entity(of: Ident) /* Eventually, this should be an e-class id instead */
//       case Raw(value: Long | Double | String | Boolean)
// }

// enum RawType {
//   case Long
//       case Double
//       case String
//       case Boolean
// }

// enum Type {
//   case Sort()
//       case Operation(argsort: Ident, ret: Type)
//       case Ref()
//       case Entity(sort: Ident)
//       case Raw(oftype: RawType)
// }

// import Data._

object Schemas {
  import Patches.{Ident}

  enum RawType {
    case Long, Double, String, Boolean
  }

  enum Type {
    case Sort()
    case Operation(argsort: Ident, ret: Either[Ident, RawType])
  }

  type Patch = Patches.Patch[Type, Void]
}
