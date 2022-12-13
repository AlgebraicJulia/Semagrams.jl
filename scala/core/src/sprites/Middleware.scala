package semagrams.sprites

import semagrams._
import com.raquo.laminar.api.L._

trait Middleware {
  def modifySignal(ent: Entity, updates: Signal[PropMap]): Signal[PropMap]
  def modifyRenderedSprite(ent: Entity, rs: RenderedSprite): Unit
}

case class WithMiddleware(
    s: Sprite,
    middleware: Seq[Middleware]
) extends Sprite {
  def render(ent: Entity, init: PropMap, updates: Signal[PropMap]) = {
    val modified =
      middleware.foldLeft(updates)((s, m) => m.modifySignal(ent, s))
    val rendered = s.render(ent, init, modified)
    for (m <- middleware) {
      m.modifyRenderedSprite(ent, rendered)
    }
    rendered
  }

  def boundaryPt(props: PropMap, dir: Complex) = {
    s.boundaryPt(props, dir)
  }
}
