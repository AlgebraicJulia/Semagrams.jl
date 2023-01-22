package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import semagrams.acsets._

case class GenericHTMLSprite(
  build: () => HtmlElement,
  globalSize: Signal[Complex]
) extends Sprite {
  def present(
    ent: Entity,
    init: ACSet,
    updates: Signal[ACSet],
    attachHandlers: HandlerAttacher
  ): SvgElement = {
    val elt = build()
    elt.amend(
      pointerEvents := "auto"
    )
    svg.foreignObject(
      xy := Complex(0,0),
      wh <-- globalSize,
      svg.pointerEvents := "none",
      div(
        styleAttr <-- globalSize.map(z => s"display: flex; width: ${z.x}px; height: ${z.y}px"),
        pointerEvents := "none",
        xmlns := "http://www.w3.org/1999/xhtml",
        elt
      )
    )
  }

  // Doesn't really make sense in this context
  def boundaryPt(data: PropMap, dir: Complex): Complex = Complex(0,0)

  def bbox(data: PropMap) = BoundingBox(0, 0)
}
