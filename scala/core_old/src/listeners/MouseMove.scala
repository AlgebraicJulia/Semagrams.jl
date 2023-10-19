package semagrams.listeners

import semagrams._
import semagrams.util._
import com.raquo.laminar.api.L._
import org.scalajs.dom
import org.scalajs.dom.SVGSVGElement

def svgCoords(elt: SVGSVGElement, evt: dom.MouseEvent): Complex = {
  val pt = elt.createSVGPoint()
  pt.x = evt.clientX
  pt.y = evt.clientY
  val svgP = pt.matrixTransform(elt.getScreenCTM().inverse())
  Complex(svgP.x, svgP.y)
}

def mouseMoveListener(eventWriter: Observer[Event]) = inContext(
  svg => onMouseMove.map(
    evt => Event.MouseMove(svgCoords(svg.ref.asInstanceOf[SVGSVGElement], evt)))
      --> eventWriter
)
