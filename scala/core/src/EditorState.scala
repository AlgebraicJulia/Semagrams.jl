package semagrams

import acsets._
import balloons._

import com.raquo.laminar.api.L._

case class EditorState(
    hovered: Option[Part],
    mousePos: Complex
) extends PureBalloon[EditorState.Event, EditorState] {
  def current = this

  import EditorState._
  import Event._
  def next(evt: Event): EditorState = evt match {
    case Hover(ent) => this.copy(hovered = Some(ent))
    case UnHover(ent) =>
      this.copy(hovered = if hovered == Some(ent) then None else hovered)
    case MouseMove(pos) => this.copy(mousePos = pos)
  }
}

object EditorState {
  enum Event {
    case Hover(part: Part)
    case UnHover(part: Part)
    case MouseMove(pos: Complex)
  }
}

// object EditorState {
//   def modifyACSet(acsetSig: Signal[Instance], stateSig: Signal[EditorState]) =
//     acsetSig
//       .combineWith(stateSig)
//       .map((acset, state) => {
//         state.hovered match {
//           case Some(ent: Part) => acset.setSubpart(ent, Hovered, ())
//           case _               => acset
//         }
//       })
// }
