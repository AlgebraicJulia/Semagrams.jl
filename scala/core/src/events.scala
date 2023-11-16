package semagrams

import acsets.Complex
import com.raquo.laminar.api.L._

import acsets._

enum MouseButton:
  case Left
  case Middle
  case Right

object MouseButton {
  def fromJS(idx: Int) = idx match {
    case 0 => Left
    case 1 => Middle
    case 2 => Right
  }
}

enum LocalEvent {
  case MouseEnter(ent: Part)
  case MouseLeave(ent: Part)
  case MouseDown(ent: Option[Part], button: MouseButton)
  case MouseUp(ent: Option[Part], button: MouseButton)
  case Click(ent: Option[Part], button: MouseButton)
  case DoubleClick(ent: Option[Part], button: MouseButton)
  case MouseLeaveBox(pos: Complex)
  case MouseMove(pos: Complex)
  case ContextMenu(ent: Option[Part])
}

enum GlobalEvent {
  case KeyDown(key: String)
  case KeyUp(key: String)
}

export GlobalEvent._
export LocalEvent._
