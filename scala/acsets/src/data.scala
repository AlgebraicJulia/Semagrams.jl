package acsets

import java.util.UUID

type EntId = UUID

type SortId = UUID

type PropId = UUID

opaque type PatchHash = Long

opaque type SchemaRevisionHash = Long
opaque type InstanceRevisionHash = Long

enum Data {
  case F(of: Double)
  case I(of: Long)
  case S(of: String)
  case B(of: Boolean)
}

enum DataType {
  case F
  case I
  case S
  case B
}

enum Value {
  case Reference(to: EntId)
  case Constant(of: Data)
  case Revision(hash: InstanceRevisionHash)
}

enum ValueType {
  case Reference(ofsort: SortId)
  case Constant(oftype: DataType)
  case Revision(ofschema: SchemaRevisionHash)
}
