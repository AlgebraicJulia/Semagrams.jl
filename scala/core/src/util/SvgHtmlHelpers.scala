package semagrams.util

import com.raquo.laminar.api.L._

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
