package semagrams.controllers

import semagrams._
import com.raquo.laminar.api.L._

case class HoverStartEvent(ent: Entity) extends EditorEvent

case class HoverEndEvent(ent: Entity) extends EditorEvent

class HoverController extends Controller[Option[Entity]] {
  type ControllerModel = Option[Entity]

  def initialModel = None

  def processEvent(model: ControllerModel, evt: EditorEvent): Outcome[ControllerModel] = {
    evt match {
      case HoverStartEvent(ent) => Outcome.success(Some(ent))
      case HoverEndEvent(ent) => if (Some(ent) == model) {
        Outcome.success(None)
      } else {
        Outcome.success(model)
      }
      case _ => Outcome.success(model)
    }
  }

  def state(model: ControllerModel) = model
}
