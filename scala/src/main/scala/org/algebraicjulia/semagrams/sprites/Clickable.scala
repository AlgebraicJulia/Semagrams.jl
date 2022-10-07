package org.algebraicjulia.semagrams.sprites

import com.raquo.laminar.api.L._
import org.algebraicjulia.semagrams._
import org.algebraicjulia.semagrams.controllers._

case class Clickable(
    mouse: MouseController,
    handle: Handle
) extends Middleware {
  override def modifyRendered(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle)
      .amend(
        mouse.clickable(ent)
      )
    rs
  }
}
