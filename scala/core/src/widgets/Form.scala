package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._
import cats.effect.IO
import semagrams.acsets.Part

/** A small menu. To be used with [[UIState.dialogue]], this returns an IO
  * action for the choice made.
  *
  * @todo
  *   It might be simpler to just return an integer for the choice made, and
  *   then have an array of IO actions on the other side.
  */
def Form[A](
    entries: Seq[(String,LensedVar[A,String])]
)(finished: Observer[Unit]) = {
  div(
    styleAttr := "display: flex; flex-direction: column; padding: 0px; border: none",
    entries.map(
      { case (text, v) =>
        div(
          div(text),
          input(
            typ := "text",
            value <-- v.signal,
            onInput.mapToValue --> v.writer
          )
        )
      }
    ),
    button(
      "Save",
      onSubmit.mapTo(()) --> finished
    )
  )
}


