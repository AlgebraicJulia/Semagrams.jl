package semagrams.controllers

import semagrams._
import com.raquo.laminar.api.L._
import semagrams.util.CustomModifier

case class HoverState(state: Option[Entity]) {
  def hover(ent: Entity) = HoverState(Some(ent))
  def leave(ent: Entity) = HoverState(
      if Some(ent) == state
      then None
      else state
  )

  def isHovered(ent: Entity) = state == Some(ent)
}

case class HoverController($state: Var[HoverState]) extends Modifier[SvgElement] {
  def hoverable[El<:SvgElement](ent: Entity) = {
    CustomModifier[El](el =>
      el.amend(
        onMouseEnter --> $state.updater((state,_) => state.hover(ent)),
        onMouseLeave --> $state.updater((state,_) => state.leave(ent)),
      )
    )
  }

  def switchState[A](ent: Entity, hovered: A, unhovered: A): Signal[A] = {
    $state.signal.map(_.isHovered(ent)).map(
      {
        case true => hovered
        case false => unhovered
      }
    )
  }
}

object HoverController {
  def apply() = {
    new HoverController(Var(HoverState(None)))
  }
}
