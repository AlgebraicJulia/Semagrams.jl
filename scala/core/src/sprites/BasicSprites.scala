package semagrams.sprites

val BasicDisc = WithMiddleware(
  Disc(),
  Seq(
    Hoverable(es.hover, MainHandle, PropMap() + (Fill, "lightgrey")),
    Clickable(es.mouse, MainHandle)
  )
)

val BasicArrow = WithMiddleware(
  Arrow(),
  Seq(
    Hoverable(es.hover, MainHandle, PropMap() + (Stroke, "lightgrey")),
    Clickable(es.mouse, MainHandle)
  )
)
