package semagrams.sprites

import semagrams._
import semagrams.util._
import com.raquo.laminar.api.L._

trait Middleware {
  def modifySignal(ent: Entity, updates: Signal[PropMap]): Signal[PropMap] =
    updates
  def wrapHandler(f: HandlerAttacher): HandlerAttacher
}

case class WithMiddleware(
    s: Sprite,
    middleware: Seq[Middleware]
) extends Sprite {
  def present(ent: Entity, init: PropMap, updates: Signal[PropMap], attachHandlers: HandlerAttacher) = {
    val modified =
      middleware.foldLeft(updates)((s, m) => m.modifySignal(ent, s))
    s.present(
      ent,
      init,
      modified,
      middleware.foldLeft(attachHandlers)((f, m) => m.wrapHandler(f))
    )
  }

  override def boundaryPt(subent: Entity, props: PropMap, dir: Complex) = {
    s.boundaryPt(subent, props, dir)
  }

  override def bbox(subent: Entity, data: PropMap) = s.bbox(subent, data)
}
