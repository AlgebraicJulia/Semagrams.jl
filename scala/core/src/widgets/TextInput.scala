package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def TextInput[A](v: LensedVar[A, String], multiline: Boolean)(
    finished: Observer[Unit]
) = {
  val common = Seq(
    value <-- v.signal,
    onInput.mapToValue --> v.writer,
    onKeyDown.stopPropagation
      .filter(k => k.key == "Escape")
      .mapTo(())
      --> finished,
    onBlur.mapTo(()) --> finished
  )
  if (multiline) {
    textArea(
      height := "100px",
      width := "150px",
      resize := "none",
      common,
      onMountCallback(el => {
        val ref = el.thisNode.ref
        ref.focus()
        // val length = ref.value.length()
        // ref.setSelectionRange(0, length)
      })
      // visibility := "hidden"
    )
  } else {
    input(
      typ := "text",
      common,
      onMountCallback(el => {
        val ref = el.thisNode.ref
        ref.focus()
        val length = ref.value.length()
        ref.setSelectionRange(0, length)
      })
    )
  }
}
