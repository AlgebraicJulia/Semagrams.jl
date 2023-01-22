package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import semagrams.controllers._
import semagrams.acsets._

case class Hoverable(
    hover: HoverController,
    acs: Var[ACSet]
) extends Middleware {
  // override def modifySignal(ent: Entity, $acs: Signal[ACSet]) = {
  //   Signal
  //     .combine(
  //       $acs,
  //       hover.switchState(ent, PropMap() + (Hovered, true), PropMap() + (Hovered, false)))
  //     .map(_.addProps(_))
  // }

  override def wrapHandler(f: HandlerAttacher) = (ent, elt) => {
    elt.amend(
      hover.hoverable(ent, acs)
    )

    f(ent, elt)
  }
}
