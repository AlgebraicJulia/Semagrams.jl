package semagrams

import com.raquo.laminar.api.L._

trait Controller[State] {
  type ControllerModel
  type StateType = State

  def initialModel: ControllerModel

  def processEvent(model: ControllerModel, evt: EditorEvent): Outcome[ControllerModel]

  def state(model: ControllerModel): State

  def globalHook(
    el: SvgElement,
    handle: ControllerHandle[State],
    updates: Signal[ControllerModel],
    bus: Observer[EditorEvent]
  ): Unit = {}
}
