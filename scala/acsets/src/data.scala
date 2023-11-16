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

enum Value {
  case C(of: Complex)
  case F(of: Double)
  case I(of: Long)
  case S(of: String)
  case B(of: Boolean)
  case Reference(to: Part)
  case Revision(hash: InstanceRevisionHash)
}

sealed trait ValueType {
  type T

  def coerce(v: Value): Option[T]

  def produce(x: T): Value
}

object ValueType {
  case object C extends ValueType {
    type T = Complex

    def coerce(d: Value): Option[T] = d match {
      case Value.C(x) => Some(x)
      case _          => None
    }

    def produce(x: T) = Value.C(x)
  }

  case object F extends ValueType {
    type T = Double

    def coerce(d: Value): Option[T] = d match {
      case Value.F(x) => Some(x)
      case _          => None
    }

    def produce(x: T) = Value.F(x)
  }

  case object I extends ValueType {
    type T = Long

    def coerce(d: Value): Option[T] = d match {
      case Value.I(x) => Some(x)
      case _          => None
    }

    def produce(x: T) = Value.I(x)
  }

  case object S extends ValueType {
    type T = String

    def coerce(d: Value): Option[T] = d match {
      case Value.S(x) => Some(x)
      case _          => None
    }

    def produce(x: T) = Value.S(x)
  }

  case object B extends ValueType {
    type T = Boolean

    def coerce(d: Value): Option[T] = d match {
      case Value.B(x) => Some(x)
      case _          => None
    }

    def produce(x: T) = Value.B(x)
  }

  case class Reference(ofsort: SortId) extends ValueType {
    type T = Part

    def coerce(v: Value): Option[T] = v match {
      case Value.Reference(p) if p.sort == ofsort => Some(p)
      case _                                      => None
    }

    def produce(x: T) = Value.Reference(x)
  }
  case class Revision(ofschema: SchemaRevisionHash) extends ValueType {
    type T = InstanceRevisionHash
    def coerce(v: Value): Option[T] = v match {
      case Value.Revision(r) => Some(r)
      case _                 => None
    }

    def produce(x: T) = Value.Revision(x)
  }
}
