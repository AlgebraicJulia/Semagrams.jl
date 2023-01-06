package semagrams.sprites

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

  def render(
      ent: Entity,
      init: PropMap,
      updates: L.Signal[PropMap]
  ): RenderedSprite = {
    val data = updates.map(defaults ++ _)
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
    val root = g(arrow, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

  def bbox(data: PropMap) = BoundingBox(0, 0)
}

object Arrow {
  val defaults = PropMap()
    + (Stroke, "black")
    + (Bend, 0)
    + (StrokeDasharray, "none")
    + (Interactable, true)

  def apply() = new Arrow(defaults)
}
