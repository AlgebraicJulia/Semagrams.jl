package semagrams.acsets

import upickle.default._

abstract class Subpart {
  type Value
}

abstract class Ob

abstract class Hom extends Subpart {
  type Value = Entity
  val codom: Ob
}

abstract class AttrType {
  type Value
  val rw: ReadWriter[Value]
}

abstract class Attr extends Subpart {
  val codom: AttrType

  type Value = codom.Value

  def writeValue(v: Any) = {
    codom.rw.transform(v.asInstanceOf[Value], ujson.Value)
  }

  def readValue(sv: ujson.Value) = {
    read[Value](sv)(codom.rw)
  }
}

trait Schema[S] {
  extension (s: S)
    def obs: Seq[Ob]

    def homs(x: Ob): Seq[Hom]

    def attrtypes: Seq[AttrType]

    def attrs(x: Ob): Seq[Attr]

    def contains(x: Ob): Boolean = obs contains x
    def contains(x: Ob, f: Hom): Boolean = homs(x) contains f
    def contains(x: AttrType): Boolean = attrtypes contains x
    def contains(x: Ob, f: Attr): Boolean = attrs(x) contains f

  val rw: ReadWriter[S]

  val obRW: ReadWriter[Ob]
  val homRW: ReadWriter[Hom]
  val attrtypeRW: ReadWriter[AttrType]
  val attrRW: ReadWriter[Attr]
}
