package semagrams

import semagrams._
import com.raquo.laminar.api.L._

abstract class Bindable {}

class EditorState(val elt: SvgElement, controllers: Seq[Controller]) {
  val bindables = EventBus[Bindable]()
  val viewports = Var(Set[Viewport]())
  val controllerMap = controllers.map(c => (c.handle, c)).toMap
  elt.amend(
    controllers,
    children <-- viewports.signal.map(_.map(_.elt).toSeq)
  )

  def controller(h: ControllerHandle): h.Controller = {
    controllerMap(h).asInstanceOf[h.Controller]
  }

  def register(v: Viewport) = {
    viewports.update(_ + v)
  }

  def deregister(v: Viewport) = {
    viewports.update(_ - v)
  }
}
