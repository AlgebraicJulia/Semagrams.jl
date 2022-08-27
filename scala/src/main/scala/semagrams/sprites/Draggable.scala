package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.acsets._
import semagrams.util._
import semagrams.controllers._

case class Draggable(
  drag: DragController,
  center: Entity => Complex,
  update: (Entity, Complex) => Unit,
  handle: Handle
) extends Middleware {
  override def modifyRendered(ent: Entity, rs: RenderedSprite) = {
    rs.handles(handle).amend(
      drag.draggable(() => center(ent), Observer(c => update(ent, c)))
    )
    rs
  }
}

object Draggable {
  def dragPart[X <: Ob: ValueOf, A: ACSet](
    drag: DragController,
    $state: Var[A],
    attr: Attr[X, Complex],
    handle: Handle): Draggable = {
      Draggable(
        drag,
        ent => $state.now().subpart(attr, ent.asInstanceOf[Elt[X]]).get,
        (ent,v) => $state.update(_.setSubpart(attr, ent.asInstanceOf[Elt[X]], v)),
        handle
      )
  }
}
