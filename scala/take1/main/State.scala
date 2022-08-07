package semagrams

import semagrams.ACSets._
import semagrams.Params._
import semagrams.Config

object State {
  case class State(
    acset: ACSet[EntityParams],
    config: Config
  )
}
