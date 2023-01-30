package semagrams

import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._
import scala.util.Random

case class BoundingBox(
  pos: Complex,
  dims: Complex
) {
  def sample(): Complex =
    Complex(Random.nextDouble() * dims.x + pos.x, Random.nextDouble() * dims.y + pos.y)
}

type HandlerAttacher = (Entity, SvgElement) => Unit

/** A Sprite contains the information necessary to turn a sub-ACSet into a
 * reactive SVG on the screen.
 *
 * TODO: Sprites should have an "injection" method for statically computing some
 * layout that they want to have available for boundaryPt/bbox queries, or simply adding
 * default properties. This injection is called before present, and the result is saved
 * in the EntityMap.
 *
 */
trait Sprite {
  def updateACSet(acs: ACSet): ACSet = acs

  def present(
      ent: Entity,
      init: ACSet,
      updates: Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): SvgElement

  def boundaryPt(
    subent: Entity,
    data: ACSet,
    dir: Complex
  ): Option[Complex] = None

  def center(
    subent: Entity,
    data: ACSet
  ): Option[Complex] = None

  def bbox(
    subent: Entity,
    data: ACSet
  ): Option[BoundingBox] = None
}
