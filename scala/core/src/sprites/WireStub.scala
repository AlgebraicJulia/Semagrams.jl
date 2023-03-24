package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.svg._

case class WireStub(defaults: PropMap, dir: Complex) extends Sprite {
  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(defaults ++ _.props)
    val stub = line(
      z1 <-- data.map(_(Center)),
      z2 <-- data.map(_(Center) + dir),
      stroke <-- data.map(d =>
        if d.get(Hovered).isDefined then "lightgrey" else d(Stroke)
      )
    )
    val offset = (dir * Complex(0, 1)).normalize * 5
    val handle = polygon(
      pointsC <-- data.map(p => {
        val c = p(Center)
        Seq(c + offset, c + dir + offset, c + dir - offset, c - offset)
      }),
      fill := "white",
      opacity := "0"
    )
    attachHandlers(ent, handle)
    g(stub, handle)
  }

  override def center(_subent: Entity, acs: ACSet) = acs.props.get(Center)
}
