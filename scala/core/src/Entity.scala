package semagrams

import upickle.default._

trait Property {
  type Value

  val rw: ReadWriter[Value]

  def writeValue(v: Any) = {
    rw.transform(v.asInstanceOf[Value], ujson.Value)
  }

  def readValue(sv: ujson.Value) = {
    read[Value](sv)(rw)
  }
}

trait PValue[T: ReadWriter] extends Property {
  type Value = T

  val rw = summon[ReadWriter[T]]
}

trait Ob

case class Entity(id: Int, ob: Ob) {
  def asElt(x: Ob) = if (x == ob) Some(this) else None
}
