package semagrams.controllers

import semagrams._
import semagrams.util._

import com.raquo.laminar.api.L._

case class DragStartEvent(h: Complex => EditorEvent) extends EditorEvent

class DragController extends Controller[Unit] {
  type ControllerModel = Option[Complex => EditorEvent]

  def initialModel = None

  def processEvent(model: ControllerModel, evt: EditorEvent): Outcome[ControllerModel] = {
    model match {
      case Some(h) => evt match {
          case MouseMoveEvent(pos) => {
            Outcome.success(model).tell(List(h(pos)))
          }
          case MouseUpEvent() => {
            Outcome.success(None)
          }
          case MouseLeaveEvent() => {
            Outcome.success(None)
          }
          case _ => {
            Outcome.success(model)
          }
        }
      case None => evt match {
        case DragStartEvent(h) => {
          Outcome.success(Some(h))
        }
        case _ => {
          Outcome.success(model)
        }
      }
    }
  }

  def state(model: ControllerModel) = {}

  override def globalHook(
    el: SvgElement,
    handle: ControllerHandle[Unit],
    updates: Signal[ControllerModel],
    bus: Observer[EditorEvent]
  ) = {
    el.amend(
      onMouseMove.map(evt => MouseMoveEvent(Complex(evt.clientX, evt.clientY))) --> bus,
      onMouseUp.map(_ => MouseUpEvent()) --> bus,
      onMouseLeave.map(_ => MouseLeaveEvent()) --> bus,
    )
  }
}
