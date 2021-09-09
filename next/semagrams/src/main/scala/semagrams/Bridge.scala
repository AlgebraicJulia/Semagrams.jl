package semagrams

import semagrams.ACSets._
import semagrams.Params._
import semagrams.Sprite._
import semagrams.Semagrams._
import semagrams.Config

import com.raquo.laminar.api.L._
import com.raquo.domtypes.generic.codecs.StringAsIsCodec
import com.raquo.laminar.nodes.ReactiveSvgElement
import org.scalajs.dom

/* This is Bridge in the sense of the bridge of a spaceship: the central command
 * It doesn't go between two things
 */
object Bridge {
  def createBridge(
    init_acset: ACSet[EntityParams],
    config: Config
  ): Div = {
    val commands = EventBus[SemagramCommand]()

    val (output, display) = createSemagram(init_acset, commands.events, config)

    val cursorPos = Var((0.0,0.0))

    val keyboardCommands = composeEvents(onKeyPress)(
      _.map(_.key)
        .withCurrentValueOf(output.exposed, cursorPos.signal)
        .collect(handleKeyboard.tupled.unlift))

    div(
      svg.svg(
        svg.width := config.width.toString,
        svg.height := config.height.toString,
        svg.customSvgAttr("tabindex", StringAsIsCodec) := "0",
        inContext(thisNode => onMouseMove.map(getCoordinates(thisNode, _)) --> cursorPos),
        keyboardCommands --> commands,
        display
      )
    )
  }

  def getCoordinates(thisNode: ReactiveSvgElement[dom.svg.SVG],
                     ev: dom.raw.MouseEvent): (Double, Double) = {
    val elt = thisNode.ref
    val pt = elt.createSVGPoint
    pt.x = ev.clientX
    pt.y = ev.clientY
    val svgP = pt.matrixTransform(elt.getScreenCTM().inverse())
    (svgP.x, svgP.y)
  }

  def handleKeyboard(key: String, exposed: SemagramExposed, cursor: (Double, Double)):
      Option[SemagramCommand] = {
    key match {
      case "a" => {
        val (x,y) = cursor
        Some(SemagramCommand.AddEntity("V", EntityParams(x,y)))
      }
      case "d" => exposed.hoveredEntity.map(SemagramCommand.RemoveEntity(_))
      case _ => None
    }
  }
}
