package semagrams.controllers

import com.raquo.laminar.api.L._
import semagrams.util._

object TipController {
  def apply() = new TipController(Var(None), "12pt")
}

case class TipController($tipText: Var[Option[List[String]]], size: String)
    extends Modifier[SvgElement] {

  def show(s: String*) = $tipText.set(Some(List(s*)))

  def hide() = $tipText.set(None)

  override def apply(el: SvgElement) = {
    val eltDims = Complex(600, 400)
    el.amend(
      child <-- $tipText.signal.map(
        _.map(texts =>
          svg.text(
            xy := Complex(15, 0),
            texts.map(text => svg.tspan(
              textToNode(text),
              svg.dy := "1em",
              svg.x := "15",
              svg.textAnchor := "start",
              svg.dominantBaseline := "hanging",
              svg.style := "user-select: none"
            )),
            svg.fontSize := size
          )
        ).getOrElse(svg.g())
      )
    )
  }
}
