package semagrams.sprites

import semagrams.Entity
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util.Complex

abstract class AbstractProperty

abstract class Property[String] extends AbstractProperty

object Fill extends Property[String]
object Stroke extends Property[String]
object InnerSep extends Property[Double]
object MinimumSize extends Property[Double]
object MinimumWidth extends Property[Double]
object MinimumHeight extends Property[Double]
object Content extends Property[String]
object Center extends Property[Complex]
object Start extends Property[Complex]
object End extends Property[Complex]

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
}

abstract class Handle

object MainHandle extends Handle

/** A RenderedSprite consists of an SVGElement that should be mounted to the
  * DOM, and a collection of SVGElements that can have event-handlers attached
  * to them, for clicking, dragging, hovering, etc.
  */
case class RenderedSprite(
    root: SvgElement,
    handles: Map[Handle, SvgElement]
)

/** A Sprite contains the information necessary to turn a PropMap into a
  * reactive SVG on the screen.
  *
  * TODO: Sprites should also turn a PropMap into TikZ code.
  */
trait Sprite {

  /** This returns a ScreenSprite because
    */
  def present(
      ent: Entity,
      init: PropMap,
      updates: Signal[PropMap]
  ): RenderedSprite

  def boundaryPt(
      ent: Entity,
      data: PropMap,
      dir: Complex
  ): Complex
}
