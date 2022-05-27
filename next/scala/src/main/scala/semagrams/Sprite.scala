package semagrams

import semagrams.Entity
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util.Complex

/*
 * A sprite is a fairly complicated beast. Here are some properties
 *
 * - A sprite has reactive state. The sprite can both receive and send updates to its state.
 * - The state of a sprite is a map of properties
 * - A sprite has named anchors. These are positions that depend on the state of the sprite, that other sprites can attach to.
 * - The boundary of a sprite can be queried at any angle
 * - A sprite can have content that it resizes dynamically in response to
 * - Sprites can be rendered in dynamic svg, or TikZ
 * - A sprite can
 *
 * The data flow goes
 *
 * Controllers -> Sprites + PropMaps -> Events -> Controllers
 */

/*
 * This should eventually represent a reference to an anchor of another sprite
 */
abstract class ExternalAnchor

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

// Sprites will use this property in different ways
// For instance, an edge sprite might use Anchor(0) and Anchor(1) for
// source and target
case class Anchor(n: Int) extends Property[ExternalAnchor]

case class PropMap(map : Map[AbstractProperty, Any]) {
  def apply[T](p: Property[T]): T = {
    map(p).asInstanceOf[T]
  }

  def get[T](p: Property[T]): Option[T] = {
    map.get(p).map(_.asInstanceOf[T])
  }

  def +[T](kv: (Property[T], T)): PropMap = {
    this.copy(map = map + kv)
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
    initProps: PropMap,
    updates: Signal[PropMap],
  ): SvgElement
}

class SpriteCollection(
  val S: Sprite,
  val sprites: Signal[List[(Entity, S.Model)]]
) extends Modifier[SvgElement] {
  def present(): Signal[List[SvgElement]] = {
    sprites.split(_._1)({ case (ent, init, updates) => S.present(ent, init._2, updates.map(_._2))})
  }

  override def apply(el: SvgElement) = el.amend(
    children <-- present()
  )
}

object SpriteCollection {
  def apply(S: Sprite, sprites: Signal[List[(Entity, S.Model)]]) = {
    new SpriteCollection(S, sprites)
  }
}
