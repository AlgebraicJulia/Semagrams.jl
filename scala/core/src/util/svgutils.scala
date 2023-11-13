package semagrams.util

import com.raquo.laminar.api.L._

import org.scalajs.dom


/* Default svg element properties */
def svgDefs(): Seq[SvgElement] =
  Seq(
    svg.marker(
      svg.idAttr := "arrowhead",
      svg.markerWidth := "10",
      svg.markerHeight := "7",
      svg.refX := "10",
      svg.refY := "3.5",
      svg.orient := "auto",
      svg.polygon(
        svg.points := "0 0, 10 3.5, 0 7"
      )
    )
  )


def svgCoords(elt: dom.SVGSVGElement, evt: dom.MouseEvent): Complex = {
  val pt = elt.createSVGPoint()
  pt.x = evt.clientX
  pt.y = evt.clientY
  val svgP = pt.matrixTransform(elt.getScreenCTM().inverse())
  Complex(svgP.x, svgP.y)
}
