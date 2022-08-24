package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import semagrams.controllers._

case class Draggable(
  drag: DragController,
  center: Entity => Signal[Complex],
  update: (Entity, Complex) => Unit,
  handle: Handle
) extends Middleware {
  override def modifyRendered(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle).amend(
      drag.draggable(center(ent), Observer(c => update(ent, c)))
    )
    rs
  }
}
