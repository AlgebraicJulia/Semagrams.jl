package semagrams.controllers

import com.raquo.laminar.api.L._
import semagrams.util._

object TipController {
  def apply(pos: Complex) = new TipController(Var(None), "12pt", pos)
}

case class TipController(
    $tipText: Var[Option[List[String]]],
    fontSize: String,
    pos: Complex
) extends Modifier[SvgElement] {

  def show(s: String*) = $tipText.set(Some(List(s*)))

  def hide() = $tipText.set(None)

  override def apply(el: SvgElement) = {
    el.amend(
      child <-- $tipText.signal.map(
        _.map(texts =>
          svg.text(
            xy := pos,
            texts.map(text =>
              svg.tspan(
                textToNode(text),
                svg.dy := "1.2em",
                svg.x := "15",
                svg.textAnchor := "start",
                svg.dominantBaseline := "hanging",
                svg.style := "user-select: none"
              )
            ),
            svg.fontSize := fontSize
          )
        ).getOrElse(svg.g())
      )
    )
  }
}
