package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import semagrams.controllers._

case class Hoverable(
    hover: HoverController,
    extraProps: PropMap
) extends Middleware {
  override def modifySignal(ent: Entity, $p: Signal[PropMap]) = {
    Signal
      .combine($p, hover.switchState(ent, extraProps, PropMap()))
      .map(_ ++ _)
  }

  override def wrapHandler(f: HandlerAttacher) = (ent, elt) => {
    elt.amend(
      hover.hoverable(ent)
    )

    f(ent, elt)
  }
}
