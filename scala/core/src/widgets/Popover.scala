package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def Popover(
    $lines: Signal[Seq[String]],
    width: Double,
    ypadding: Double,
    innerSep: Double
) = {
  val eltDims = Complex(600, 400)
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
              textToNode(line),
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
