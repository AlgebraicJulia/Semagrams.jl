package org.algebraicjulia.semagrams.sprites

import com.raquo.laminar.api.L._
import org.algebraicjulia.semagrams._
import org.algebraicjulia.semagrams.util._
import org.algebraicjulia.semagrams.controllers._

case class Hoverable(
    hover: HoverController,
    handle: Handle,
    extraProps: PropMap
) extends Middleware {
  override def updatePropsS(ent: Entity, $p: Signal[PropMap]) = {
    Signal
      .combine($p, hover.switchState(ent, extraProps, PropMap()))
      .map(_ ++ _)
  }

  override def modifyRendered(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle)
      .amend(
        hover.hoverable(ent)
      )
    rs
  }
}
