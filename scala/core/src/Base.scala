package semagrams

import com.raquo.laminar.api.L._
import cats.effect._
import org.scalajs.dom
import scala.scalajs.js.annotation._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec

def baseSvg() = {
  svg.svg(
    svg.height := "100%",
    svg.width := "100%",
    svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
  )
}

abstract class Semagram {
  def run(es: EditorState): IO[Unit]

  @JSExport
  def main(div: dom.Element) = {
    val base = baseSvg()
    val es = new EditorState(
      base
    )
    render(div, base)
    run(es).unsafeRunAndForget()(unsafe.IORuntime.global)
  }
}
