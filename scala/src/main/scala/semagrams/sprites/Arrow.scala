package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

case class Arrow() extends Sprite {
  def blockPts(s: Complex, e: Complex, d: Double): Seq[Complex] = {
    val dz = (s - e).normalize * Complex(0,1) * d
    Seq(s + dz, s - dz, e - dz, e + dz)
  }

  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    val arrow = line(
      x1 <-- $p.map(_(Start).x.toString),
      y1 <-- $p.map(_(Start).y.toString),
      x2 <-- $p.map(_(End).x.toString),
      y2 <-- $p.map(_(End).y.toString),
      stroke <-- $p.map(_(Stroke)),
      markerEnd := "url(#arrowhead)"
    )
    val handle = polygon(
      fill := "white",
      opacity := "0",
      pointsC <-- $p.map(p => blockPts(p(Start), p(End), 10))
    )
    val root = g(arrow, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(ent: Entity, data: PropMap, dir: Complex): Complex =
    Complex(0, 0)
}
