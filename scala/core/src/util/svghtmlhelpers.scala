package semagrams.util

import com.raquo.laminar.api.L._
import semagrams.util.Complex


/** Make an SVG element containing the html object.
  *
  * We have to specify manually the dimensions and position of the html, because
  * SVG does not have automatic layout (unlike html).
  *
  * One way of getting "automatic layout" is to make an html element which takes
  * up the entire semagrams window but is transparent and doesn't intercept
  * mouse actions, and then doing layout within that.
  */
def wrappedHtml(el: Element, pos: Signal[Complex], dims: Signal[Complex]) = {
  svg.foreignObject(
    xy <-- pos,
    wh <-- dims,
    div(
      xmlns := "http://www.w3.org/1999/xhtml",
      el
    )
  )
}
