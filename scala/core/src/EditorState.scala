package semagrams

import acsets._
import balloons._

import semagrams.util._
import com.raquo.laminar.api.L._

case class EditorState(
    hovered: Option[Part],
    mousePos: Complex
) extends PureBalloon[LocalEvent, EditorState] {
  def current = this

  def next(evt: LocalEvent): EditorState = evt match {
    case MouseEnter(ent) => this.copy(hovered = Some(ent))
    case MouseLeave(ent) =>
      this.copy(hovered = if hovered == Some(ent) then None else hovered)
    case MouseMove(pos) => this.copy(mousePos = pos)
    case _              => this
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
