package semagrams.sprites

import semagrams._

def BasicDisc(es: EditorState) = WithMiddleware(
  Disc(),
  Seq(
    Hoverable(es.hover, MainHandle, PropMap() + (Fill, "lightgrey")),
    Clickable(es.mouse, MainHandle)
  )
)

def BasicArrow(es: EditorState) = WithMiddleware(
  Arrow(),
  Seq(
    Hoverable(es.hover, MainHandle, PropMap() + (Stroke, "lightgrey")),
    Clickable(es.mouse, MainHandle)
  )
)
