package semagrams.sprites

import semagrams.acsets._
import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.util._

case class Arrow(defaults: PropMap) extends Sprite {
  def blockPath(
      s: Complex,
      e: Complex,
      d: Double,
      bend: Double
  ): Seq[Path.Element] = {
    import Path.Element._
    val dz = (s - e).normalize * Complex(0, 1) * d
    curvedPath(s + dz, e + dz, bend)
      ++ Seq(LineTo(e - dz))
      ++ curvedPath(e - dz, s - dz, -bend)
      ++ Seq(LineTo(s + dz), ClosePath)
  }

  def curvedPath(s: Complex, e: Complex, bend: Double): Seq[Path.Element] = {
    import Path.Element._
    val rot = Complex(0, bend).exp
    val cs = rot * (s * (-1 / 4) + e * (1 / 4)) + s
    val ce = rot.cong * (s * (1 / 4) + e * (-1 / 4)) + e
    Seq(MoveTo(s), Cubic(cs, ce, e))
  }

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(defaults ++ _.props)
    val arrow = path(
      pathElts <-- data.map(p => curvedPath(p(Start), p(End), p(Bend))),
      stroke <-- data.map(_(Stroke)),
      strokeDashArray <-- data.map(_(StrokeDasharray)),
      fill := "none",
      markerEnd := "url(#arrowhead)",
      pointerEvents := "none"
    )
    val handle = path(
      fill := "white",
      opacity := "0",
      pathElts <-- data.map(p => blockPath(p(Start), p(End), 5, p(Bend))),
      pointerEvents <-- data.map(p =>
        if p(Interactable) then "auto" else "none"
      )
    )
    attachHandlers(ent, handle)
    g(arrow, handle)
  }
}

object Arrow {
  val defaults = PropMap()
    + (Stroke, "black")
    + (Bend, 0)
    + (StrokeDasharray, "none")
    + (Interactable, true)

  def apply() = new Arrow(defaults)
}
