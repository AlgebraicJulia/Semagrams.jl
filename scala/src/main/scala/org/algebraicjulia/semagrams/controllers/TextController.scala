package org.algebraicjulia.semagrams.controllers

import com.raquo.laminar.api.L._
import org.algebraicjulia.semagrams.util._
import scala.scalajs.js

object TextController {
  enum State:
    case Editing(
        listener: Observer[String],
        init: String,
        cb: Option[() => Unit]
    )
    case Closed()

  import State._

  def makeTextBox(
      pos: Complex,
      dims: Complex,
      state: State,
      escape: EventBus[Unit]
  ) = {
    state match {
      case Editing(listener, init, _) => {
        svg.foreignObject(
          xy := pos,
          wh := dims,
          div(
            xmlns := "http://www.w3.org/1999/xhtml",
            input(
              typ := "text",
              onInput.mapToValue --> listener,
              onKeyDown.stopPropagation.filter(_.key == "Escape").mapTo(())
                --> escape.writer,
              onBlur.mapTo(()) --> escape.writer,
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
      case Closed() => {
        svg.g()
      }
    }
  }

  case class Settings(
      dims: Complex
  )

  val defaultSettings = Settings(Complex(200, 40))

  def apply() = {
    new TextController(Var(Closed()), defaultSettings)
  }
}

case class TextController(
    $state: Var[TextController.State],
    settings: TextController.Settings
) extends Modifier[SvgElement] {

  import TextController._
  import State._

  def editText(listener: Observer[String], init: String) = {
    $state.set(Editing(listener, init, None))
  }

  def editTextBlocking(
      listener: Observer[String],
      init: String,
      cb: () => Unit
  ) = {
    $state.set(Editing(listener, init, Some(cb)))
  }

  override def apply(element: SvgElement): Unit = {
    val eltDims = Complex(600, 400)

    val escape = EventBus[Unit]

    val pos = Complex(eltDims.x / 2, eltDims.y)
      - Complex(settings.dims.x / 2, settings.dims.y)

    element.amend(
      svg.g(
        child <-- $state.signal.map(makeTextBox(pos, settings.dims, _, escape))
      ),
      escape.events --> $state.updater({ case (oldS, _) =>
        oldS match {
          case Editing(_, _, Some(cb)) => { cb(); Closed() }
          case _                       => Closed()
        }
      }),
      escape.events --> Observer(_ =>
        element.ref.asInstanceOf[js.Dynamic].focus()
      )
    )
  }
}
