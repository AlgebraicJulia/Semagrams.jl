package semagrams.sprites

import semagrams._
import semagrams.util._

def BasicWrapper(hoverProps: PropMap)(sprite: Sprite)(es: EditorState) = WithMiddleware(
  sprite,
  Seq(
    Hoverable(es.hover, hoverProps),
    Clickable(es.mouse)
  )
)

val BasicArrow = BasicWrapper(PropMap() + (Stroke, "lightgrey"))(Arrow())

val BasicDisc = BasicWrapper(PropMap() + (Fill, "lightgrey"))(Disc())

val BasicRect = BasicWrapper(PropMap() + (Fill, "lightgrey"))(Rect())

val BasicWire = BasicWrapper(PropMap() + (Fill, "lightgrey"))(Wire())