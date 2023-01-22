package semagrams.sprites

import semagrams._
import semagrams.util._
import semagrams.acsets._

import com.raquo.laminar.api.L._

def BasicWrapper(sprite: Sprite)(es: EditorState, acs: Var[ACSet]) = WithMiddleware(
  sprite,
  Seq(
    Hoverable(es.hover, acs),
    Clickable(es.mouse)
  )
)

val BasicArrow = BasicWrapper(Arrow())

val BasicDisc = BasicWrapper(Disc())

val BasicRect = BasicWrapper(Rect())

val BasicWire = BasicWrapper(Wire())

def BasicDPBox(inPort: Ob, outPort: Ob) = BasicWrapper(
  DPBox(
    Rect(),
    WireStub(PropMap() + (Stroke, "black"), -10),
    WireStub(PropMap() + (Stroke, "black"), 10),
    inPort,
    outPort
  )
)

def BasicWireStub(extend: Double) = BasicWrapper(WireStub(PropMap() + (Stroke, "black"), extend))
