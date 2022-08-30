package semagrams.controllers

import semagrams._
import com.raquo.laminar.api.L._
import semagrams.util.CustomModifier

/** The hover controller keeps track of what entity is currently hovered by the
  * mouse. Commands might use the currently hovered entity for certain things.
  */

/** The state of the HoverController: either hovering over an entity, or nothing
  * is hovered.
  */
case class HoverState(state: Option[Entity]) {
  def hover(ent: Entity) = HoverState(Some(ent))
  def leave(ent: Entity) = HoverState(
    // Only unhover if we are actually leaving the entity
    // that we were hovering over in the first place.
    if Some(ent) == state
    then None
    else state
  )

  def isHovered(ent: Entity) = state == Some(ent)
}

case class HoverController($state: Var[HoverState])
    extends Modifier[SvgElement] {

  /** This makes a certain SVG record hovering
    */
  def hoverable[El <: SvgElement](ent: Entity) = List(
    onMouseEnter --> $state.updater((state, _) => state.hover(ent)),
    onMouseLeave --> $state.updater((state, _) => state.leave(ent))
  )

  /** This most commonly would be used to style a certain entity based on
    * whether or not it was hovered.
    */
  def switchState[A](ent: Entity, hovered: A, unhovered: A): Signal[A] =
    $state.signal.map(state =>
      if state.isHovered(ent) then hovered else unhovered
    )
}

object HoverController {
  // By default, we start with nothing hovered.
  def apply() = new HoverController(Var(HoverState(None)))
}
