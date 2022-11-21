package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

case class Arrow() extends Sprite {
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
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    val arrow = path(
      pathElts <-- $p.map(p => curvedPath(p(Start), p(End), p(Bend))),
      stroke <-- $p.map(_(Stroke)),
      strokeDashArray <-- $p.map(_.get(StrokeDasharray).getOrElse("none")),
      fill := "none",
      markerEnd := "url(#arrowhead)"
    )
    val handle = path(
      fill := "white",
      opacity := "0",
      pathElts <-- $p.map(p => blockPath(p(Start), p(End), 5, p(Bend)))
    )
    val root = g(arrow, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)
}
