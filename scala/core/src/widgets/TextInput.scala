package semagrams.controllers

import com.raquo.laminar.api.L._
import semagrams.util._

def TextInput(listener: Observer[String], init: String, finished: Observer[Unit], dims: Complex) = {
  val eltDims = Complex(600, 400)

  val pos = Complex(eltDims.x / 2, eltDims.y)
    - Complex(dims.x / 2, dims.y)

  svg.foreignObject(
    xy := pos,
    wh := dims,
    div(
      xmlns := "http://www.w3.org/1999/xhtml",
      input(
        typ := "text",
        onInput.mapToValue --> listener,
        onKeyDown.stopPropagation.filter(Set("Enter", "Escape") contains _.key).mapTo(())
          --> finished,
        onBlur.mapTo(()) --> finished,
        defaultValue := init,
        onMountCallback(el => {
          val ref = el.thisNode.ref
          ref.focus()
          val length = ref.value.length()
          ref.setSelectionRange(0, length)
        })
      )
    )
  )
}

