package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

case class Arrow() extends Sprite {
  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    val root = line(
      x1 <-- $p.map(_(Start).x.toString),
      y1 <-- $p.map(_(Start).y.toString),
      x2 <-- $p.map(_(End).x.toString),
      y2 <-- $p.map(_(End).y.toString),
      stroke <-- $p.map(_(Stroke)),
      markerEnd := "url(#arrowhead)"
    )
    RenderedSprite(root, Map())
  }

  def boundaryPt(ent: Entity, data: PropMap, dir: Complex): Complex =
    Complex(0, 0)
}
