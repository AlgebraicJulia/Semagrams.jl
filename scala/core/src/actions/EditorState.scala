package semagrams.actions

import semagrams._
import semagrams.util._
import semagrams.controllers._
import semagrams.widgets._

import com.raquo.laminar.api.L._

case class EditorState[Model](
    mouse: MouseController,
    drag: DragController,
    hover: HoverController,
    keyboard: KeyboardController,
    transform: TransformController,
    bindables: EventBus[Any],
    $model: Var[Model],
    elt: SvgElement,
    playArea: SvgElement,
    controlChildCommands: Observer[ChildrenCommand],
    relativeChildCommands: Observer[ChildrenCommand],
    update: () => Unit
) {

  def dims(): Complex = Complex(elt.ref.clientWidth, elt.ref.clientHeight)
}

object EditorState {
  def apply[Model]($model: Var[Model], elt: SvgElement, update: () => Unit) = {
    val transform = TransformController()
    val mouse = MouseController(transform)
    val drag = DragController(mouse)
    val hover = HoverController()
    val keyboard = KeyboardController()
    val bindables = EventBus[Any]()
    val controlCommandBus = EventBus[ChildrenCommand]()
    val relativeCommandBus = EventBus[ChildrenCommand]()
    val playArea = svg.g(
      transform,
      children.command <-- relativeCommandBus.events
    )

    val controlArea = svg.g(
      children.command <-- controlCommandBus.events
    )

    elt.amend(
      mouse,
      drag,
      hover,
      keyboard,
      playArea,
      controlArea,
      mouse.mouseEvents --> bindables,
      keyboard.keydowns --> bindables,
      keyboard.keyups --> bindables
    )

    new EditorState(
      mouse,
      drag,
      hover,
      keyboard,
      transform,
      bindables,
      $model,
      elt,
      playArea,
      controlCommandBus.writer,
      relativeCommandBus.writer,
      update
    )
  }
}
