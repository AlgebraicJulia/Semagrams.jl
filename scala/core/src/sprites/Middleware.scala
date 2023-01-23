package semagrams.sprites

import semagrams._
import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._

trait Middleware {
  def modifySignal(ent: Entity, updates: Signal[ACSet]): Signal[ACSet] =
    updates
  def wrapHandler(f: HandlerAttacher): HandlerAttacher
}

case class WithMiddleware(
    s: Sprite,
    middleware: Seq[Middleware]
) extends Sprite {
  def present(ent: Entity, init: ACSet, updates: Signal[ACSet], attachHandlers: HandlerAttacher) = {
    val modified =
      middleware.foldLeft(updates)((s, m) => m.modifySignal(ent, s))

    s.present(
      ent,
      init,
      modified,
      middleware.foldLeft(attachHandlers)((f, m) => m.wrapHandler(f))
    )
  }

  override def boundaryPt(subent: Entity, acs: ACSet, dir: Complex) = {
    s.boundaryPt(subent, acs, dir)
  }

  override def center(subent: Entity, acs: ACSet) = {
    s.center(subent, acs)
  }

  override def bbox(subent: Entity, acs: ACSet) = s.bbox(subent, acs)
}
