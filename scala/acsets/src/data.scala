package acsets

import java.util.UUID

case class SortId(name: String)

given Ordering[SortId] = new Ordering[SortId] {
  def compare(x: SortId, y: SortId) = Ordering[String].compare(x.name, y.name)
}

case class PropId(name: String)

given Ordering[PropId] = new Ordering[PropId] {
  def compare(x: PropId, y: PropId) = Ordering[String].compare(x.name, y.name)
}

type PartId = UUID

case class Part(id: PartId, sort: SortId)

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

  def check(d: Data): Boolean = (this, d) match {
    case (F, Data.F(_)) => true
    case (I, Data.I(_)) => true
    case (S, Data.S(_)) => true
    case (B, Data.B(_)) => true
    case _              => false
  }
}

enum Value {
  case Constant(of: Data)
  case Revision(hash: InstanceRevisionHash)
  case Reference(to: Part)
}

enum ValueType {
  case Reference(ofsort: SortId)
  case Constant(oftype: DataType)
  case Revision(ofschema: SchemaRevisionHash)

  def check(v: Value): Boolean = (this, v) match {
    case (Constant(dtype), Value.Constant(d))  => dtype check d
    case (Reference(sort), Value.Reference(p)) => p.sort == sort
    case (Revision(_), Value.Revision(_))      => true
    case _                                     => true
  }
}
