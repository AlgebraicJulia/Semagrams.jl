package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def TextInput(
    listener: Observer[String],
    init: String,
    finished: Observer[Unit],
    dims: Complex,
    center: Complex
) = {
  val pos = center - (dims / 2)

  wrappedHtml(
    input(
      typ := "text",
      onInput.mapToValue --> listener,
      onKeyDown.stopPropagation
        .filter(Set("Enter", "Escape") contains _.key)
        .mapTo(())
        --> finished,
      onBlur.mapTo(()) --> finished,
      defaultValue := init,
      onMountCallback(el => {
        val ref = el.thisNode.ref
        ref.focus()
        val length = ref.value.length()
        ref.setSelectionRange(0, length)
      })
    ),
    pos,
    dims
  )
}
