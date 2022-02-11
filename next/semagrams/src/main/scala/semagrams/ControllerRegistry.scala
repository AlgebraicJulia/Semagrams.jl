package semagrams

import java.util.UUID
import semagrams.Controller
import com.raquo.laminar.api.L._
import cats.implicits._
import scala.collection.immutable.ListMap
import scala.collection.mutable

case class ControllerHandle[State](id: UUID)

case class ControllerRegistry(stateMap: ListMap[UUID, (Controller[_], Any)]) {
  def state[State](handle: ControllerHandle[State]): State = {
    val res = stateMap(handle.id)
    val ctlr = res._1
    val model = res._2.asInstanceOf[ctlr.ControllerModel]
    ctlr.state(model).asInstanceOf[State]
  }


  def processEvent(evt: EditorEvent): Outcome[ControllerRegistry] = {
    for {
      newStateMap <- stateMap.toList.traverse(
        { case (key,(ctlr,modelRaw)) => {
           val model = modelRaw.asInstanceOf[ctlr.ControllerModel]
           for {
             newModel <- ctlr.processEvent(model, evt)
           } yield (key,(ctlr,newModel))
         }
        }
      )
    } yield ControllerRegistry(ListMap(newStateMap:_*))
  }

  def globalHooks(
    el: SvgElement,
    updates: Signal[ControllerRegistry],
    bus: Observer[EditorEvent]
  ) = {
    for ((id, (ctlr, _)) <- stateMap.toList) {
      val $model = updates.map(reg => reg.stateMap(id).asInstanceOf[ctlr.ControllerModel])
      val handle = ControllerHandle[ctlr.StateType](id)
      ctlr.globalHook(el, handle, $model, bus)
    }
  }
}

object ControllerRegistry {
  def apply(controllers: List[Controller[_]]) = {
    new ControllerRegistry(
      ListMap(controllers.map(
        ctlr => (UUID.randomUUID(), (ctlr, ctlr.initialModel))
      ):_*)
    )
  }
}

case class RegistryBuilder(stateMap: mutable.ListMap[UUID, (Controller[_], Any)]) {
  def addController[State](ctlr: Controller[State]): ControllerHandle[State] = {
    val id = UUID.randomUUID()
    stateMap.addOne((id, (ctlr, ctlr.initialModel)))
    ControllerHandle[State](id)
  }

  def build(): ControllerRegistry = {
    new ControllerRegistry(ListMap(stateMap.toList:_*))
  }
}

object RegistryBuilder {
  def apply() = new RegistryBuilder(mutable.ListMap())
}
