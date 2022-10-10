package semagrams.controllers

import com.raquo.laminar.api.L._
import semagrams.util._

object PopoverController {
  def apply(width: Double, ypadding: Double, innerSep: Double) =
    new PopoverController(Var(None), width, ypadding, innerSep)
}

case class PopoverController(
    $state: Var[Option[Seq[String]]],
    width: Double,
    ypadding: Double,
    innerSep: Double
) extends Modifier[SvgElement] {

  def show(s: String*) = $state.set(Some(List(s*)))

  def hide() = $state.set(None)

  override def apply(el: SvgElement) = {
    val eltDims = Complex(600, 400)
    val pos = Complex((eltDims.x - width) / 2, ypadding)
    el.amend(
      child <-- $state.signal.map(_.map(lines => {
        svg.g(
          svg.rect(
            xy := pos,
            wh := Complex(width, eltDims.y - 2 * ypadding),
            svg.fill := "white",
            svg.stroke := "black"
          ),
          svg.text(
            xy := pos + Complex(innerSep, innerSep),
            lines
              .map(line => if line == "" then " " else line)
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
      }).getOrElse(svg.g()))
    )
  }
}
