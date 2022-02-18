package semagrams.controllers

import semagrams._
import semagrams.util._

import com.raquo.laminar.api.L._

object DragController {
  def processEvent(state: Option[Observer[Complex]], evt: MouseEvent): Option[Observer[Complex]] = {
    import MouseEvent._
    state.flatMap(h =>
      evt match {
        case MouseMove(pos) => {
          h.onNext(pos)
          Some(h)
        }
        case MouseUp(_, _) => {
          None
        }
        case MouseLeave(_) => {
          None
        }
        case _ => {
          Some(h)
        }
      })
  }

  def apply(mouse: MouseController) = {
    new DragController(Var(None), mouse)
  }
}

case class DragController(
  $state: Var[Option[Observer[Complex]]],
  mouse: MouseController
) extends Modifier[SvgElement] {

  def drag(updates: Observer[Complex]) = $state.set(Some(updates))

  def draggable[El<:SvgElement](
    center: Source[Complex],
    updates: Observer[Complex]
  ) = {
    val curCenter = Var(Complex(0,0))
    CustomModifier[El](el =>
      el.amend(
        center --> curCenter.writer,
        onMouseDown.map(
          ev => {
            val c = curCenter.now()
            val init = Complex(ev.clientX, ev.clientY)
            val offset = c - init
            Some(updates.contramap[Complex](offset + _))
          }) --> $state.writer
      )
    )
  }

  override def apply(el: SvgElement) = el.amend(
    mouse.mouseEvents --> $state.updater(DragController.processEvent)
  )
}
