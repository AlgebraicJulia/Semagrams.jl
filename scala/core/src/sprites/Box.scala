package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

case object InPorts extends PValue[Seq[Int]]
case object OutPorts extends PValue[Seq[Int]]

case class Box(props: PropMap) extends Sprite {
  val rectSprite = Rect(props)

  def present(
    ent: Entity,
    init: PropMap,
    updates: L.Signal[PropMap],
    attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val rect = rectSprite.present(ent, init, updates, attachHandlers)

    g(rect)
  }
}
