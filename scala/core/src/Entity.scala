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
    entities: (A, EntityMap) => Seq[(Entity, Sprite, PropMap)]
) {
  def addEntities(a: A, m: EntityMap) = {
    m ++ entities(a, m).map((e, s, p) => (e, (s, p))).toMap
  }

  def withProps(props: PropMap) = EntitySource[A](
    entities(_, _).map((e, s, p) => (e, s, p ++ props))
  )

  def addPropsBy(f: (Entity, PropMap, EntityMap) => PropMap) =
    EntitySource[A]((a, m) =>
      entities(a, m).map((e, s, p) => (e, s, p ++ f(e, p, m)))
    )
}

case object Background extends Entity
