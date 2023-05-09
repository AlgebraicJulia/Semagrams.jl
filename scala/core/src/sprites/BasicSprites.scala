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

def BasicWire(src:Hom,tgt:Hom) = BasicWrapper(Wire(src,tgt))

def BasicDPBox(inPort: Ob, outPort: Ob,
  portStyle: (ACSet,Part )=> PropMap
)(es: EditorState) = DPBox(
  BasicRect(es),
  BasicWireStub(-10)(es),
  BasicWireStub(10)(es),
  inPort,
  outPort,
  portStyle
)

def BasicWireStub(extend: Double) = BasicWrapper(
  WireStub(PropMap() + (Stroke, "black"), extend)
)

def AltDPBox(inPort: Ob, outPort: Ob, 
  portStyle: (ACSet,Part) => PropMap = (_,_) => PropMap()
)(es: EditorState): DPBox = DPBox(
  BasicWrapper(Rect(PropMap() + (MinimumHeight,80.0) + (MinimumWidth,50.0)))(es),
  BasicPort()(es),
  BasicPort()(es),
  inPort,
  outPort,
  portStyle
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
