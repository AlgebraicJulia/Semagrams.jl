package semagrams

import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

/** This creates the svg element that will all of the Semagrams activity
  *
  * @todo:
  *   there should be more customization here
  * @todo:
  *   markers should come from elsewhere
  */
def baseSvg() = {
  svg.svg(
    svg.height := "100%",
    svg.width := "100%",
    svg.svgAttr("tabindex", StringAsIsCodec, None) := "-1",
    svg.style := "border:black;" +
      "border-style:solid;" +
      "background-color:white;" +
      "box-sizing: border-box;",
    svg.defs(
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
  )
}
