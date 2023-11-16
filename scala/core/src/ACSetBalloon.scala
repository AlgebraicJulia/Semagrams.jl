package semagrams

import acsets._
import balloons._

case class ACSetBalloon(
    current: Instance.Clean
) extends PureBalloon[Instance.Patch, Instance.Clean] {
  def next(msg: Instance.Patch) =
    ACSetBalloon(Instance.applyPatch(current, msg).get)
}
