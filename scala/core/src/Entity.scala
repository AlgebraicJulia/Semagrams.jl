package semagrams

import upickle.default._

abstract class Property {
  type Value

  val rw: ReadWriter[Value]

  def writeValue(v: Any) = {
    rw.transform(v.asInstanceOf[Value], ujson.Value)
  }

  def readValue(sv: ujson.Value) = {
    read[Value](sv)(rw)
  }
}

abstract class Ob

case class Entity(id: Int, ob: Ob) {
  def asElt(x: Ob) = if (x == ob) Some(this) else None
}
