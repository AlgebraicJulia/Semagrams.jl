package semagrams

import com.raquo.laminar.api.L._
import cats.effect._
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import com.raquo.domtypes.generic.codecs.StringAsIsCodec

def baseSvg() = {
  svg.svg(
    svg.height := "100%",
    svg.width := "100%",
    svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
    svg.style := "border:black;border-style:solid;background-color:white",
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

abstract class Semagram {
  def run(es: EditorState, init: Option[String]): IO[Unit]

  @JSExport
  def main(div: dom.Element, init: js.UndefOr[String]) = {
    val base = baseSvg()
    val es = new EditorState(
      base
    )
    render(div, base)
    run(es, init.toOption).unsafeRunAndForget()(unsafe.IORuntime.global)
  }
}
