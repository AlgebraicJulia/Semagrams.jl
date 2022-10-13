package semagrams.sprites

import semagrams.util.Complex
import upickle.default._

abstract class AbstractProperty {
  type Value
  val rw: ReadWriter[Value]
}

abstract class Property[T: ReadWriter] extends AbstractProperty {
  override type Value = T
  val rw: ReadWriter[T] = summon[ReadWriter[T]]
}

case object Fill extends Property[String]
case object Stroke extends Property[String]
case object InnerSep extends Property[Double]
case object MinimumSize extends Property[Double]
case object MinimumWidth extends Property[Double]
case object MinimumHeight extends Property[Double]
case object FontSize extends Property[Double]
case object Content extends Property[String]
case object Center extends Property[Complex]
case object Start extends Property[Complex]
case object End extends Property[Complex]
case object Bend extends Property[Double]
case object Style extends Property[String]

val allProperties: List[AbstractProperty] = List(
  Fill,
  Stroke,
  InnerSep,
  MinimumSize,
  MinimumWidth,
  MinimumHeight,
  Content,
  Center,
  Start,
  End
)

val propByName = allProperties.map(p => (p.toString, p)).toMap

case class PropMap(map: Map[AbstractProperty, Any]) {
  def apply[T](p: Property[T]): T = {
    map(p).asInstanceOf[T]
  }

  def get[T](p: Property[T]): Option[T] = {
    map.get(p).map(_.asInstanceOf[T])
  }

  def +[T](kv: (Property[T], T)): PropMap = {
    this.copy(map = map + kv)
  }

  def ++(other: PropMap): PropMap = {
    this.copy(map = map ++ other.map)
  }
}

object PropMap {
  def apply() = {
    new PropMap(Map[AbstractProperty, Any]())
  }

  val rw: ReadWriter[PropMap] = summon[ReadWriter[ujson.Obj]].bimap(
    props =>
      ujson.Obj.from(
        props.map
          .map({ case (prop, v) =>
            (
              prop.toString,
              prop.rw.transform(v.asInstanceOf[prop.Value], ujson.Value)
            )
          })
          .toList
      ),
    obj =>
      PropMap(
        obj.value
          .map(
            {
              case (s, v) => {
                val prop = propByName(s)
                (prop, read(v)(prop.rw))
              }
            }
          )
          .toMap
      )
  )
}
