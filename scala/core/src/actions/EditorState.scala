package semagrams.actions

import semagrams._
import semagrams.util._
import semagrams.controllers._
import semagrams.widgets._

import com.raquo.laminar.api.L._
import cats.effect.kernel.Unique.Token

// TODO:
// There should be a map of controllers, and a map of viewports which can be
// updated dynamically.
// Then each viewport has its own transform, and mouse events can be mapped into
// the coordinates of each viewport.
// Handles and Entities should be merged.
// And there should be "entity sources", which produce entities. One entity
// source is an ACSet wrapped in entity produces, but another could just be a
// map of entities.  Also, we should lose "entityType".

trait ElementHandle

case class AnonHandle(t: Token) extends ElementHandle

case class EditorState[Model](
    mouse: MouseController,
    drag: DragController,
    hover: HoverController,
    keyboard: KeyboardController,
    transform: TransformController,
    bindables: EventBus[Any],
    $model: Var[Model],
    elt: SvgElement,
    sceneElements: Var[Map[ElementHandle, Element]],
    controlElements: Var[Map[ElementHandle, Element]],
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
    val sceneElements = Var(Map[ElementHandle, Element]())
    val controlElements = Var(Map[ElementHandle, Element]())
    val playArea = svg.g(
      transform,
      children <-- sceneElements.signal.map(_.values.toSeq)
    )

    val controlArea = svg.g(
      children <-- controlElements.signal.map(_.values.toSeq)
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
      sceneElements,
      controlElements,
      update
    )
  }
}
