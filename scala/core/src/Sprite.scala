package semagrams

import semagrams.util._
import com.raquo.laminar.api.L._

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
  def render(
      ent: Entity,
      init: PropMap,
      updates: Signal[PropMap]
  ): RenderedSprite

  def present(
      ent: Entity,
      init: PropMap,
      updates: Signal[PropMap]
  ): SvgElement = render(ent, init, updates).root

  def boundaryPt(
      data: PropMap,
      dir: Complex
  ): Complex
}
