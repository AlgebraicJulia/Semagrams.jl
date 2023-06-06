package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

/** Returns a multiline text element that updates according to a signal.
  *
  * Useful for prompts and tutorials.
  */
def Tip(
    $tipText: Signal[Seq[String]],
    fontSize: String,
    pos: Complex
) = {
  svg.text(
    xy := pos,
    svg.fontSize := fontSize,
    children <-- $tipText.map(
      _.map(line =>
        svg.tspan(
          textToTextNode(line),
          svg.dy := "1.2em",
          svg.x := "15",
          svg.textAnchor := "start",
          svg.dominantBaseline := "hanging",
          svg.style := "user-select: none"
        )
      )
    )
  )
}
