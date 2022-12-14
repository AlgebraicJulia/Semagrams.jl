package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.controllers._

case class Clickable(
    mouse: MouseController,
    handle: Handle
) extends Middleware {
  override def modifyRenderedSprite(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle)
      .amend(
        mouse.clickable(ent)
      )
  }
}
