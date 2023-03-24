package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util._
import cats.effect.IO
import semagrams.acsets.Part

def Menu[A](choices: Seq[(String, Part => IO[A])])(finished: Observer[Part => IO[A]]) = {
  div(
    styleAttr := "display: flex; flex-direction: column; padding: 0px; border: none",
    choices.map(
      {
        case (text, v) =>
          button(
            onClick.mapTo(v) --> finished,
            styleAttr := "padding: 2px; margin: 0px; border: none; background: lightgray; border: solid thin",
            a(textToNode(text))
          )
      }
    )
  )
}

