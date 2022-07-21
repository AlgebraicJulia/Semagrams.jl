package semagrams

import semagrams.Entity
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util.Complex

abstract class AbstractProperty

abstract class Property[String] extends AbstractProperty

case class Fill() extends Property[String]
case class Stroke() extends Property[String]
case class InnerSep() extends Property[Double]
case class MinimumSize() extends Property[Double]
case class MinimumWidth() extends Property[Double]
case class MinimumHeight() extends Property[Double]
case class Content() extends Property[String]
case class Center() extends Property[Complex]

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
  def apply[T]() = {
    new PropMap(Map[AbstractProperty, Any]())
  }
}

trait Sprite {
  def present(
    ent: Entity,
    init: PropMap,
    updates: Signal[PropMap],
  ): SvgElement

  def boundaryPt(
    data: PropMap,
    dir: Double
  ): Complex
}

/**
 * Sprites should also possibly have default properties?
 */
