package semagrams.widgets

import com.raquo.laminar.api.L._

/** Returns a menu where choices are associated with values passed back,
  * typically via [[UIState.dialogue]]
  */
def Select[A](choices: Seq[(String, A)])(finished: Observer[A]) = {
  div(
    styleAttr := "display: flex; flex-direction: column; padding: 10px; border: solid",
    choices.map(
      { case (text, v) =>
        div(
          onClick.mapTo(v) --> finished,
          styleAttr := "padding: 10px; margin: 5px; border: solid",
          a(textToTextNode(text))
        )
      }
    )
  )
}
