package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def wrappedHtml(el: Element, pos: Complex, dims: Complex) = {
  svg.foreignObject(
    xy := pos,
    wh := dims,
    div(
      xmlns := "http://www.w3.org/1999/xhtml",
      el
    )
  )
}
