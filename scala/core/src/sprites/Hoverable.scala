package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import semagrams.controllers._
import semagrams.acsets._

case class Hoverable(
    hover: HoverController,
) extends Middleware {
  override def modifySignal(ent: Entity, $acs: Signal[ACSet]) = {
    Signal
      .combine(
        $acs,
        hover.switchState(ent, PropMap() + (Hovered, ()), PropMap()))
      .map(_.addProps(_))
  }

  override def wrapHandler(f: HandlerAttacher) = (ent, elt) => {
    elt.amend(
      hover.hoverable(ent)
    )

    f(ent, elt)
  }
}
