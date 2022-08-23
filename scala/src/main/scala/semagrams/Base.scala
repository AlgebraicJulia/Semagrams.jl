package semagrams

import com.raquo.laminar.api.L._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec
import semagrams.actions.*
import org.scalajs.dom

def baseSvg = svg.svg(
  svg.width := "400",
  svg.height := "400",
  svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
  svg.style := "border:black;border-style:solid",
)

def mountWithAction[Model](id: String, initModel: Model, action: Action[Model, Unit]) = {
  dom.document.addEventListener(
    "DOMContentLoaded",
    (_: dom.Event) => {
      val parentDiv = dom.document.getElementById(id)
      val appElement = baseSvg

      render(parentDiv, appElement)

      val $model = Var(initModel)
      val editorState = EditorState($model, appElement)

      runAction(editorState, action)
    })
}
