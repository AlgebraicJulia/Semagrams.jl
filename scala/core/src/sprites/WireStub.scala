package semagrams.sprites

import semagrams._
import semagrams.util._

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.svg._

case class WireStub(defaults: PropMap, dir: Complex) extends Sprite {
  def present(
      ent: Entity,
      init: PropMap,
      updates: L.Signal[PropMap],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(defaults ++ _)
    val stub = line(
      z1 <-- data.map(_(Center)),
      z2 <-- data.map(_(Center) + dir),
      stroke <-- data.map(_(Stroke))
    )
    val offset = (dir * Complex(0,1)).normalize * 5
    val handle = polygon(
      pointsC <-- data.map(
        p => {
          val c = p(Center)
          Seq(c + offset, c + dir + offset, c + dir - offset, c - offset)
        }
      ),
      fill := "white",
      opacity := "0"
      // pointerEvents <-- data.map(p =>
      //   if p(Interactable) then "auto" else "none"
      // )
    )
    attachHandlers(ent, handle)
    g(stub, handle)
  }
}
