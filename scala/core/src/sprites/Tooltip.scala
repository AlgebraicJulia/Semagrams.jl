package semagrams.sprites

import com.raquo.laminar.api.L._

import semagrams._
import semagrams.actions._
import semagrams.controllers._
import semagrams.util._

case object TooltipHandle extends ElementHandle

case object TooltipEntityType extends EntityType

case object TooltipEntity extends Entity {
  val entityType = TooltipEntityType
}

def makeTooltip(text: String, mouse: MouseController): SvgElement = {
  val pm = PropMap()
    + (Content, text)
    + (MinimumHeight, 20)
    + (MinimumWidth, 40)
    + (Center, mouse.$state.now().pos - Complex(0, 20))
    + (Fill, "white")
    + (Stroke, "black")
    + (FontSize, 12)
    + (InnerSep, 5)

  Box().present(TooltipEntity, PropMap(), Val(pm)).root
}

case class Tooltip(
    handle: Handle,
    text: Entity => String,
    mouse: MouseController,
    // TODO: This should be a viewport controller
    sceneElements: Var[Map[ElementHandle, Element]],
) extends Middleware {
  override def modifyRendered(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle)
      .amend(
        onMouseOver --> sceneElements.updater(
          (m,_) => m + (TooltipHandle -> makeTooltip(text(ent), mouse))),
        onMouseOut --> sceneElements.updater(
          (m,_) => m - TooltipHandle)
      )
    rs
  }
}
