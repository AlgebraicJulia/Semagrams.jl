package semagrams

import semagrams.acsets.abstr._
import semagrams.util._
import com.raquo.laminar.api.L._

case class EditorState(
    hovered: Option[Entity],
    mousePos: Complex,
    dims: Complex
) {
  def processEvent(evt: Event): EditorState = evt match {
    case MouseEnter(ent) => 
      this.copy(hovered = Some(ent))
    case MouseLeave(ent) =>
      this.copy(hovered = if ent == Background() then None else Some(Background()))
    case MouseMove(pos) => this.copy(mousePos = pos)
    case Resize(newsize) => this.copy(dims = newsize)
    case _              => this
  }
}

object EditorState {
  def modifyACSet[A:ACSet](acsetSig: Signal[A], stateSig: Signal[EditorState]) =
    acsetSig.combineWith(stateSig).map(
      (acset, state) => {
        state.hovered match {
          case Some(ent: Part) => 
            acset.setProp(Hovered, ent,  ())
          case _ => 
            acset
        }
      }
    )
}
