package semagrams

import semagrams.util._

case class EditorState(
  hovered: Option[Entity],
  mousePos: Complex
)
