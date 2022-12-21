package semagrams.sprites

import com.raquo.laminar.api.L._
import semagrams._
import semagrams.util._
import com.raquo.airstream.core.Signal

case class GenericHTMLSprite(
  build: () => HtmlElement,
  globalSize: Signal[Complex]
) extends Sprite {
  def render(ent: Entity, init: PropMap, updates: Signal[PropMap]): RenderedSprite = {
    val elt = build()
    elt.amend(
      pointerEvents := "auto"
    )
    val wrapper =
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
    RenderedSprite(wrapper, Map(MainHandle -> wrapper))
  }

  // Doesn't really make sense in this context
  def boundaryPt(data: PropMap, dir: Complex): Complex = Complex(0,0)
}
