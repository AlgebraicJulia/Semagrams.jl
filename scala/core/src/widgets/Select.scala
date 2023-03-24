package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._

def Select[A](choices: Seq[(String, A)])(finished: Observer[A]) = {
  div(
    styleAttr := "display: flex; flex-direction: column; padding: 10px; border: solid",
    choices.map(
      {
        case (text, v) =>
          div(
            onClick.mapTo(v) --> finished,
            styleAttr := "padding: 10px; margin: 5px; border: solid",
            a(textToNode(text))
          )
      }
    )
  )
}
