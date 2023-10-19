package semagrams

import semagrams.util.Complex
import com.raquo.laminar.api.L._

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

enum Event {
  case MouseEnter(ent: Entity)
  case MouseLeave(ent: Entity)
  case MouseDown(ent: Option[Entity], button: MouseButton)
  case MouseUp(ent: Option[Entity], button: MouseButton)
  case Click(ent: Option[Entity], button: MouseButton)
  case DoubleClick(ent: Option[Entity], button: MouseButton)
  case MouseLeaveBox(pos: Complex)
  case MouseMove(pos: Complex)
  case KeyDown(key: String)
  case KeyUp(key: String)
  case ContextMenu(ent: Option[Entity])
}

enum Message {
  case EditEntity(ent: Entity)
}

export Event._