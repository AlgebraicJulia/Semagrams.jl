package semagrams.actions

import semagrams._
import semagrams.acsets._
import semagrams.controllers._
import semagrams.util._
import semagrams.widgets._

import com.raquo.laminar.api.L._

case class EditorState[Model](
    mouse: MouseController,
    drag: DragController,
    hover: HoverController,
    keyboard: KeyboardController,
    bindables: EventBus[Any],
    $model: Var[Model],
    elt: SvgElement,
    playArea: SvgElement,
    childCommands: Observer[ChildrenCommand],
    update: () => Unit
) {

  def dims(): Complex = Complex(elt.ref.clientWidth, elt.ref.clientHeight)
}

object EditorState {
  def apply[Model]($model: Var[Model], elt: SvgElement, update: () => Unit) = {
    val mouse = MouseController()
    val drag = DragController(mouse)
    val hover = HoverController()
    val keyboard = KeyboardController()
    val bindables = EventBus[Any]()
    val commandBus = EventBus[ChildrenCommand]()
    val playArea = svg.g(
      children.command <-- commandBus.events
    )

    elt.amend(
      mouse,
      drag,
      hover,
      keyboard,
      playArea,
      mouse.mouseEvents --> bindables,
      keyboard.keydowns --> bindables,
      keyboard.keyups --> bindables
    )

    new EditorState(
      mouse,
      drag,
      hover,
      keyboard,
      bindables,
      $model,
      elt,
      playArea,
      commandBus.writer,
      update
    )
  }
}
