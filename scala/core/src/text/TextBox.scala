package semagrams.text

import org.scalajs.dom
import semagrams.util._
import typings.katex

import scala.collection.mutable

val measurementStyle = """
position: absolute;
visibility: hidden;
height: auto;
width: auto;
white-space: nowrap;
font-family: Alegreya Sans, sans-serif;
"""

private val sizeCache = mutable.Map[(String, Double), Complex]()

def boxSize(text: String, fontSize: Double) = {
  if (sizeCache contains (text, fontSize)) {
    sizeCache((text, fontSize))
  } else {
    val div = renderKatex(text)
    dom.document.body.appendChild(div)
    val size = Complex(div.clientWidth + 1, div.clientHeight + 1)
    dom.document.body.removeChild(div)
    sizeCache.addOne((text, fontSize) -> size)
    size
  }
}

def renderKatex(text: String) = {
  val div = dom.document.createElement("div")
  katex.mod.default.render(text, div.asInstanceOf[dom.HTMLElement])
  div
}
