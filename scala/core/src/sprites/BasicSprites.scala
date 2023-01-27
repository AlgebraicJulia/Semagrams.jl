package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

import com.raquo.laminar.api.L._

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


def BasicWireStub(extend: Double) = BasicWrapper(WireStub(PropMap() + (Stroke, "black"), extend))
