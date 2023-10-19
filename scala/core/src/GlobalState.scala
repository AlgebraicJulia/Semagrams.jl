package semagrams

import balloons._

case class GlobalState(
    modifiers: Set[KeyModifier]
) extends PureBalloon[GlobalEvent, GlobalState] {
  def current = this

  def next(evt: GlobalEvent) = evt match {
    case GlobalEvent.KeyDown(key) =>
      KeyModifier.fromString.get(key) match {
        case Some(mod) => GlobalState(modifiers + mod)
        case None      => this
      }
    case GlobalEvent.KeyUp(key) =>
      KeyModifier.fromString.get(key) match {
        case Some(mod) => GlobalState(modifiers - mod)
        case None      => this
      }
  }
}
