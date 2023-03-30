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
    Complex(
      Random.nextDouble() * dims.x + pos.x,
      Random.nextDouble() * dims.y + pos.y
    )
}

type HandlerAttacher = (Entity, SvgElement) => Unit

/** A Sprite contains the information necessary to turn a sub-ACSet into a
  * reactive SVG on the screen.
  *
  * TODO: Sprites should have an "injection" method for statically computing
  * some layout that they want to have available for boundaryPt/bbox queries, or
  * simply adding default properties. This injection is called before present,
  * and the result is saved in the EntityMap. Currently, each sprite has some
  * custom code for being able to have defaults; that should not be custom
  * because then it is inconsistent.
  */
trait Sprite {

  /** Construct an SvgElement for a subacset.
    *
    * This is the main method to override in an implementation of [[Sprite]].
    *
    * SVG is an absolute medium, so what is returned here is precisely what is
    * shown on the screen; it is not moved or scaled (except possibly by a
    * global transform). Thus, this must use the properties given to fill in all
    * of the necessary parts of the sprite.
    *
    * @param ent
    *   This is the entity that is associated with this Sprite. We pass this
    *   into `present` because `present` attaches handlers to the generated svg
    *   that need to report events that reference `ent`.
    *
    * @param init
    *   This is the initial value of the subacset that specifies the properties
    *   for the sprite. In most cases, we are just interested in the top-level
    *   properties, but sometimes we use the parts inside to, for instance,
    *   display ports. Most of the time, one should get properties from
    *   `updates`, as those will change over time, but there may be some things
    *   that you need in order to construct the svg that will never change, and
    *   those can be taken from `init`.
    *
    * @param updates
    *   This is the same data as `init`, except changing over time. This should
    *   be used to, for instance, set the center of the Sprite, because then the
    *   center will change when the Sprite is dragged.
    *
    * @param attachHandlers
    *   This is used by [[MiddleWare]] to inject event handlers into the sprite.
    *   This should be called on the svg element that the mouse interacts with
    *   (which may be different from the top-level svg element).
    */
  def present(
      ent: Entity,
      init: ACSet,
      updates: Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): SvgElement

  /** Compute a point on the geometrical boundary of the sprite
    *
    * Note that this does not reference the constructed SVG element, so this can
    * be called with no side effects or global state.
    *
    * This might not make sense, so is optional to implement.
    */
  def boundaryPt(
      subent: Entity,
      data: ACSet,
      dir: Complex
  ): Option[Complex] = None

  /** Compute the geometric center of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def center(
      subent: Entity,
      data: ACSet
  ): Option[Complex] = None

  /** Compute the bounding box of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def bbox(
      subent: Entity,
      data: ACSet
  ): Option[BoundingBox] = None
}
