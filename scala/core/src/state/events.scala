package semagrams.state

import semagrams.util.Complex
import com.raquo.laminar.api.L._
import semagrams.rendering.EntityTag

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
  case MouseEnter(ent: EntityTag)
  case MouseLeave(ent: EntityTag)
  case MouseDown(ent: Option[EntityTag], button: MouseButton)
  case MouseUp(ent: Option[EntityTag], button: MouseButton)
  case Click(ent: Option[EntityTag], button: MouseButton)
  case DoubleClick(ent: Option[EntityTag], button: MouseButton)
  case MouseLeaveBox(pos: Complex)
  case MouseMove(pos: Complex)
  case KeyDown(key: String)
  case KeyUp(key: String)
  case ContextMenu(ent: Option[EntityTag])
  case Resize(size: Complex)
  case MsgEvent[Model](msg: Message[Model])
  case Blur()
}

export Event._
