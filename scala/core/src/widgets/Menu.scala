package semagrams.widgets

import com.raquo.laminar.api.L._
import cats.effect.IO

/** A small menu. To be used with [[UIState.dialogue]], this returns an IO
  * action for the choice made.
  *
  * @todo
  *   It might be simpler to just return an integer for the choice made, and
  *   then have an array of IO actions on the other side.
  */
def Menu[K, A](
    choices: Seq[(String, K => IO[A])]
)(finished: Observer[Option[K => IO[A]]]) = {
  div(
    styleAttr := "display: flex; flex-direction: column; padding: 0px; border: none",
    choices.map(
      { case (text, v) =>
        div(
          onClick.mapTo(Some(v)) --> finished,
          styleAttr := "padding: 2px; margin: 0px; border: none; background: lightgray; border: solid thin",
          a(textToTextNode(text))
        )
      }
    )
  )
}
