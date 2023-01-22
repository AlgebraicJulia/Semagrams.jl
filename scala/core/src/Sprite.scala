package semagrams

import semagrams.util._
import com.raquo.laminar.api.L._

case class BoundingBox(
  pos: Complex,
  dims: Complex
)

type HandlerAttacher = (Entity, SvgElement) => Unit

/** A Sprite contains the information necessary to turn a PropMap into a
  * reactive SVG on the screen.
  *
  * TODO: Sprites should also turn a PropMap into TikZ code.
  */
trait Sprite {
  def present(
      ent: Entity,
      init: PropMap,
      updates: Signal[PropMap],
      attachHandlers: HandlerAttacher
  ): SvgElement

  def boundaryPt(
    subent: Entity,
    data: PropMap,
    dir: Complex
  ): Option[Complex] = None

  def bbox(
    subent: Entity,
    data: PropMap
  ): Option[BoundingBox] = None
}
