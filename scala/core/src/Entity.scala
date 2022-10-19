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

trait EntityType

trait Entity {
  val entityType: EntityType
  val hash: Int

  def withType(ty: EntityType) =
    if entityType == ty then Some(this) else None
}

object BackgroundType extends EntityType

object Background extends Entity {
  val entityType = BackgroundType
  val hash = 0
}
