package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.controllers._

/** A [[Middleware]] that attaches click handlers to a sprite */
case class Clickable(
    mouse: MouseController
) extends Middleware {
  override def wrapHandler(f: HandlerAttacher) = (ent, elt) => {
    elt.amend(
      mouse.clickable(ent)
    )
    f(ent, elt)
  }
}
