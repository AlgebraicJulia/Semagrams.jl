package semagrams

import com.raquo.laminar.api.L._

trait Controller extends Modifier[SvgElement] {
  val handle: ControllerHandle
}

trait ControllerHandle {
  type Controller
}
