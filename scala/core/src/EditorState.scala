package semagrams

import semagrams.acsets.abstr._
import semagrams.util._
import com.raquo.laminar.api.L._

import org.scalajs.dom
import org.scalajs.dom.KeyboardEvent

case class EditorState(
  hovered: Option[Entity],
  dims: Complex,
  mousePos: Complex,
  selected: Seq[Part],
  modifiers: Set[KeyModifier]
) {
  import KeyModifier._
  import MouseButton._

  def processEvent(evt: Event): EditorState = evt match {
    /* Mouse events */
    case MouseEnter(ent) => 
      this.copy(hovered = Some(ent))
    case MouseLeave(ent) =>
      this.copy(hovered = if ent == backgroundPart then None else Some(backgroundPart))
    case MouseMove(pos) => this.copy(mousePos = pos)
    /* Dimension events */
    case Resize(newsize) => 
      println("resize")
      this.copy(dims = newsize)
    /* Keyboard events */    
    case KeyDown("Escape") => this.copy(selected = Seq())
    case KeyDown(key) => KeyModifier.fromString.get(key) match
      case Some(mod) => this.copy(modifiers = modifiers + mod)
      case None => this
    case KeyUp(key) => KeyModifier.fromString.get(key) match
      case Some(mod) => this.copy(modifiers = modifiers - mod)
      case None => this
    /* Clear key modifiers when focus is lost */
    case Blur() => this.copy(modifiers = Set())
    /* Selection events */
    case MouseDown(Some(part),Left) if modifiers.contains(Ctrl) => 
      if selected.contains(part)
      then this.copy(selected = selected diff Seq(part))
      else this.copy(selected = selected :+ part)
    case MouseDown(Some(part),Left) => 
      this.copy(selected = Seq(part))
    case DoubleClick(Some(part),Left) => this.copy(selected = Seq(part))
    case _              => this
  }
}

object EditorState {
  def modifyACSet[A:ACSet](acsetSig: Signal[A], stateSig: Signal[EditorState]) =
    acsetSig.combineWith(stateSig).map( (acset, state) => 
      state.hovered.collect{case p:Part => p}
        .map(ent => acset.setProp(Hovered, ent,  ()))
        .getOrElse(acset)
        .setProp(Selected,state.selected.map(_ -> ()))
    )

  def listen(into: Observer[Event]) = {
    dom.document.addEventListener("keydown",(ev:KeyboardEvent) => into.onNext(KeyDown(ev.key)))
    dom.document.addEventListener("keyup",(ev:KeyboardEvent) => into.onNext(KeyUp(ev.key)))
    dom.window.addEventListener("blur",_ => into.onNext(Blur()))
  }

}


sealed trait EditorMsg extends Message[EditorState]

case class ResizeMsg(z:Complex) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    dims = z
  )