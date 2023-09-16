package semagrams.util

import org.scalajs.dom
import semagrams.util._

import scala.collection.mutable

private val measurementStyle = """
position: absolute;
visibility: hidden;
height: auto;
width: auto;
white-space: nowrap;
font-family: Alegreya Sans, sans-serif;
"""

private val sizeCache = mutable.Map[(String, Double), Complex]()

/** Returns the dimensions of the bounding box for `text` at `fontSize`
  *
  * This caches computations, so that repeated queries are fast.
  *
  * @todo
  *   we hardcode most of the style for this; we should make this more flexible.
  */
def boxSize(text: String, fontSize: Double): Complex = {
  if (sizeCache contains (text, fontSize)) {
    sizeCache((text, fontSize))
  } else {
    val div = dom.document.createElement("div")
    div.setAttribute("style", measurementStyle + s"font-size: ${fontSize}pt;")
    div.innerText = text
    dom.document.body.appendChild(div)
    val size = Complex(div.clientWidth + 1, div.clientHeight + 1)
    dom.document.body.removeChild(div)
    sizeCache.addOne((text, fontSize) -> size)
    size
  }
}

def boxSize(text:Option[String],fontSize:Option[Double]): Complex = boxSize(
  text.getOrElse(""),
  fontSize.getOrElse(12.0)
)
