package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

import com.raquo.laminar.api.L._

/** Adds hover and click handlers to a sprite */
def BasicWrapper(sprite: Sprite)(es: EditorState) = WithMiddleware(
  sprite,
  Seq(
    Hoverable(es.hover),
    Clickable(es.mouse)
  )
)

val BasicArrow = BasicWrapper(Arrow())

val BasicDisc = BasicWrapper(Disc())

val BasicRect = BasicWrapper(Rect())

val BasicWire = BasicWrapper(Wire())

def BasicDPBox(inPort: Ob, outPort: Ob)(es: EditorState) = DPBox(
  BasicRect(es),
  BasicWireStub(-10)(es),
  BasicWireStub(10)(es),
  inPort,
  outPort
)

def BasicWireStub(extend: Double) = BasicWrapper(
  WireStub(PropMap() + (Stroke, "black"), extend)
)

def AltDPBox(inPort: Ob, outPort: Ob)(es: EditorState) = DPBox(
  BasicWrapper(Rect(PropMap() + (MinimumHeight, 80.0) + (MinimumWidth, 50.0)))(
    es
  ),
  BasicPort(PropMap() + (Fill, "red"))(es),
  BasicPort(PropMap() + (Fill, "green"))(es),
  inPort,
  outPort
)

def BasicPort(props: PropMap = PropMap()) = BasicWrapper(
  Disc(
    PropMap()
      + (Fill, "black")
      + (Stroke, "none")
      + (FontSize, 12)
      + (InnerSep, 3)
      + (OuterSep, 0)
      + (MinimumWidth, 20)
      + (MinimumHeight, 20)
      ++ props
  )
)
