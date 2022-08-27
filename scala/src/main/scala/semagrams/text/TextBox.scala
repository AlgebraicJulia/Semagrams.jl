package semagrams.text

import org.scalajs.dom
import semagrams.util._

val measurementStyle = """
position: absolute;
visibility: hidden;
height: auto;
width: auto;
white-space: nowrap;
"""

def boxSize(text: String, fontSize: Double) = {
  val div = dom.document.createElement("div")
  div.setAttribute("style", measurementStyle + s"font-size:$fontSize;")
  div.innerText = text
  dom.document.body.appendChild(div)
  val size = Complex(div.clientHeight + 1, div.clientWidth + 1)
  dom.document.body.removeChild(div)
  size
}
