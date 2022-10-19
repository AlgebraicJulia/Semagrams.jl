package semagrams.controllers

import com.raquo.laminar.api.L._
import semagrams.util._

case class TransformController($state: Var[Transform])
    extends Modifier[SvgElement] {
  override def apply(el: SvgElement) = {
    el.amend(
      svg.transform <-- $state.signal.map(_.toSvg)
    )
  }

  def screenToLogical(p: Complex) = $state.now().inverse(p)

  def logicalToScreen(p: Complex) = $state.now()(p)
}

object TransformController {
  def apply() = new TransformController(Var(Transform(1, 0)))
}
