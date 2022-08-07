package semagrams

import com.raquo.laminar.api.L._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec

def baseSvg = svg.svg(
  svg.width := "400",
  svg.height := "400",
  svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
)
