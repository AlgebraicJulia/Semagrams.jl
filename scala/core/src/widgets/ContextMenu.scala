package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._
import cats.effect.IO

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
          textToNode(line),
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


case class MenuEntry(
  text: String,
  action: IO[Unit]
)

// def ContextMenu(
//   $position: Signal[Complex],
//   $entries:Signal[Seq[MenuEntry]]
// ) = wrappedHtml(div("Hi",fill:="red"),$position,Complex(50,25))