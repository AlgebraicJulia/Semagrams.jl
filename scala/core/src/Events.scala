package semagrams

import semagrams.util.Complex

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
  case MouseDown(ent: Option[Entity], button: MouseButton)
  case MouseUp(ent: Option[Entity], button: MouseButton)
  case DoubleClick(ent: Option[Entity], button: MouseButton)
  case MouseLeave(pos: Complex)
  case MouseMove(pos: Complex)
  case KeyDown(key: String)
  case KeyUp(key: String)
}

export Event._
