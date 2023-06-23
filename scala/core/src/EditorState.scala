package semagrams

import semagrams.util._

case class EditorState(
  hovered: Option[Entity],
  mousePos: Complex
) {
  def processEvent(evt: Event): EditorState = evt match {
    case MouseEnter(ent) => this.copy(hovered = Some(ent))
    case MouseLeave(ent) => this.copy(hovered = if hovered == Some(ent) then None else hovered)
    case MouseMove(pos) => this.copy(mousePos = pos)
    case _ => this
  }
}
