package semagrams

import semagrams.acsets._
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

trait EntityType

trait Entity {
  val ty: EntityType

  def extend(sub: Entity): Entity = SubEntity(this, sub)
}

case class SubEntity(parent: Entity, child: Entity) extends Entity {
  val ty = SubEntityType(parent.ty, child.ty)
}

case class SubEntityType(parentTy: EntityType, childTy: EntityType) extends EntityType

type EntityMap = Map[Entity, (Sprite, ACSet)]

object EntityMap {
  def apply(): EntityMap = Map[Entity, (Sprite, ACSet)]()
}

case class EntitySource[A](
    entities: (A, EntityMap) => Seq[(Entity, Sprite, ACSet)]
) {
  def addEntities(a: A, m: EntityMap) = {
    m ++ entities(a, m).map((e, s, p) => (e, (s, p))).toMap
  }

  def withProps(props: PropMap) = EntitySource[A](
    entities(_, _).map((e, s, a) => (e, s, a.addProps(props)))
  )

  def addPropsBy(f: (Entity, ACSet, EntityMap) => PropMap) =
    EntitySource[A]((g, m) =>
      entities(g, m).map((e, s, a) => (e, s, a.addProps(f(e, a, m))))
    )

  def updateEntities(f: (Entity, ACSet) => (Entity, ACSet)) = {
    EntitySource[A]((g, m) =>
      entities(g, m).map((e, s, a) => { val (e1, a1) = f(e,a); (e1, s, a1) })
    )
  }
}

case class Background() extends Entity {
  val ty = Background
}

object Background extends EntityType
