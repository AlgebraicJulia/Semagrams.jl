package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

/** A text box that displays over the window. */
def Popover(
    $lines: Signal[Seq[String]],
    width: Double,
    ypadding: Double,
    innerSep: Double,
    eltDims: Complex
) = {
  val pos = Complex((eltDims.x - width) / 2, ypadding)
  svg.g(
    svg.rect(
      xy := pos,
      wh := Complex(width, eltDims.y - 2 * ypadding),
      svg.fill := "white",
      svg.stroke := "black"
    ),
    svg.text(
      xy := pos + Complex(innerSep, innerSep),
      children <-- $lines.map(
        _.map(line => if line == "" then " " else line)
          .map(line =>
            svg.tspan(
              textToTextNode(line),
              svg.dy := "1.2em",
              svg.x := (pos.x + innerSep).toString,
              svg.textAnchor := "start",
              svg.dominantBaseline := "hanging",
              svg.style := "user-select: none"
            )
          )
      )
    )
  )
}
