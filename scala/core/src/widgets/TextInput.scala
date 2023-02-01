package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def TextInput[A](v: LensedVar[A,String], multiline: Boolean)(finished: Observer[Unit]) = {
  val common = Seq(
    value <-- v.signal,
    onInput.mapToValue --> v.writer,
    onKeyDown.stopPropagation
      .filter(Set("Enter", "Escape") contains _.key)
      .mapTo(())
      --> finished,
    onBlur.mapTo(()) --> finished,
  )
  if (multiline) {
    textArea(
      height := "100%",
      width := "400px",
      resize := "none",
      common
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
