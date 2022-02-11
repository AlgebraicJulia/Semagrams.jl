package semagrams

import semagrams.util.Complex


abstract class EditorEvent {}

abstract class MouseEvent extends EditorEvent {}

case class MouseMoveEvent(pos: Complex) extends MouseEvent

case class MouseUpEvent() extends MouseEvent

case class MouseLeaveEvent() extends MouseEvent

case class MoveEntityEvent(ent: Entity, pos: Complex) extends EditorEvent
