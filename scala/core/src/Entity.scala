package semagrams

import upickle.default._
import com.raquo.laminar.api.L._

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

trait Entity

type EntityMap = Map[Entity, (Sprite, PropMap)]

object EntityMap {
  def apply(): EntityMap = Map[Entity, (Sprite, PropMap)]()
}

case class EntitySource[A](
    entities: (A, EntityMap) => EntityMap
) {
  def addEntities(a: A, m: EntityMap) = {
    m ++ entities(a, m)
  }

  def withProps(props: PropMap) = EntitySource[A](
    entities(_, _).view.mapValues((s, p) => (s, p ++ props)).toMap
  )

  def addPropsBy(f: (Entity, PropMap, EntityMap) => PropMap) =
    EntitySource[A]((a, m) =>
      entities(a, m).map({ case (e, (s, p)) => (e, (s, p ++ f(e, p, m))) })
    )
}

case object Background extends Entity
