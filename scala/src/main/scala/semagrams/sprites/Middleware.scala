package semagrams.sprites

import semagrams.Entity
import com.raquo.laminar.api.L.{*, given}
import semagrams.util.Complex

/**
 * The purpose of sprite middleware is to make a variety of components that can
 * be flexibly combined in order to give features to sprites.
 *
 * It does this by inserting extra properties before the sprite is rendered,
 * and then modifying the rendered sprite.
 */

trait Middleware {
  def updateProps(ent: Entity, p: PropMap): PropMap = p

  /**
   * This should only be overridden to add extra properties to the sprite that
   * are for the purpose of interaction, because any properties added here will
   * not be in TikZ output, and will not be used to compute boundaries.
   */
  def updatePropsS(ent: Entity, $p: Signal[PropMap]): Signal[PropMap] = $p.map(updateProps(ent, _))

  def modifyRendered(ent: Entity, s: RenderedSprite): RenderedSprite = s
}

case class WithMiddleware(s: Sprite, m: Middleware) extends Sprite {
  def present(ent: Entity, p: PropMap, $p: Signal[PropMap]) = {
    val q = m.updateProps(ent, p)
    val $q = m.updatePropsS(ent, $p)
    val rendered = s.present(ent, q, $q)
     m.modifyRendered(ent, rendered)
  }

  def boundaryPt(ent: Entity, p: PropMap, dir: Double): Complex = {
    s.boundaryPt(ent, m.updateProps(ent, p), dir)
  }
}

case class Stack(ms: List[Middleware]) extends Middleware {
  override def updateProps(ent: Entity, p: PropMap): PropMap =
    ms.foldLeft(p)((q,m) => m.updateProps(ent, q))

  override def updatePropsS(ent: Entity, $p: Signal[PropMap]): Signal[PropMap] =
    ms.foldLeft($p)(($q,m) => m.updatePropsS(ent, $q))

  override def modifyRendered(ent: Entity, s: RenderedSprite): RenderedSprite =
    ms.foldRight(s)((m,r) => m.modifyRendered(ent, r))
}

object Stack {
  def apply(ms: Middleware*) = new Stack(List(ms*))
}
