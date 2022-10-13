package semagrams

import com.raquo.laminar.api.L._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec
import semagrams.actions.*
import org.scalajs.dom
import scala.scalajs.js
import upickle.default._

def baseSvg = svg.svg(
  svg.width := "600",
  svg.height := "400",
  svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
  svg.style := "border:black;border-style:solid;background-color:white",
  svg.defs(
    svg.marker(
      svg.idAttr := "arrowhead",
      svg.markerWidth := "10",
      svg.markerHeight := "7",
      svg.refX := "10",
      svg.refY := "3.5",
      svg.orient := "auto",
      svg.polygon(
        svg.points := "0 0, 10 3.5, 0 7"
      )
    )
  )
)

def mountWithAction[Model](
    parentDiv: dom.Element,
    initModel: Model,
    serializer: ReadWriter[Model],
    action: Action[Model, Unit]
) = {
  val $model = Var(initModel)
  val appElement = baseSvg

  js.Object.defineProperty(
    parentDiv,
    "value",
    new {
      override val get = () => write($model.now())(serializer)
      override val set =
        (newVal) => $model.set(read(newVal.asInstanceOf[String])(serializer))
    }
  )

  val update: () => Unit = () => {
    parentDiv.dispatchEvent(dom.CustomEvent("input", null))
  }

  val editorState = EditorState($model, appElement, update)

  render(parentDiv, appElement)

  runAction(editorState, action)
}
