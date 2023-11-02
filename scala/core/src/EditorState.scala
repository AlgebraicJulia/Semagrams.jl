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

  def eventMsg(evt:Event): Message[EditorState] = evt match
    /* Mouse events */
    case MouseEnter(ent) => HoverMsg(hovered,Some(ent))
    case MouseLeave(ent) => HoverMsg(hovered, 
      if ent == backgroundPart then None else Some(backgroundPart)
    )
    case MouseMove(pos) => FreeMsg(_.copy(mousePos = pos))
    /* Dimension events */
    case Resize(newsize) => FreeMsg(_.copy(dims = newsize))
    /* Keyboard events */    
    case KeyDown("Escape") => FreeMsg(_.copy(selected = Seq()))
    case KeyDown(key) => KeyModifier.fromString.get(key) match
      case Some(mod) => ModMsg(modifiers,modifiers + mod)
      case None => Message()
    case KeyUp(key) => KeyModifier.fromString.get(key) match
      case Some(mod) => ModMsg(modifiers,modifiers - mod)
      case None => Message()
    /* Clear key modifiers when focus is lost */
    case Blur() => ModMsg(modifiers,Set())
    /* Selection events */
    case MouseDown(Some(part),Left) if modifiers.contains(Ctrl) => 
      if selected.contains(part)
      then SelectMsg(selected,selected.filter(_ == part))
      else SelectMsg(selected,selected :+ part)
    case MouseDown(Some(part),Left) if part == backgroundPart & selected.nonEmpty =>
      SelectMsg(selected,Seq())
    case MouseDown(Some(part),Left) if part != backgroundPart =>
      SelectMsg(selected,Seq(part))
 
    case _ => Message()
  

  def processEvent(evt: Event): EditorState = 
    eventMsg(evt).execute(this)


}

object EditorState {


  def listen(into: Observer[Event]) = {
    dom.document.addEventListener("keydown",(ev:KeyboardEvent) => into.onNext(KeyDown(ev.key)))
    dom.document.addEventListener("keyup",(ev:KeyboardEvent) => into.onNext(KeyUp(ev.key)))
    dom.window.addEventListener("blur",_ => into.onNext(Blur()))
  }

}


sealed trait EditorMsg extends AtomicMessage[EditorState]

case class ResizeMsg(z:Complex) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    dims = z
  )

case class HoverMsg(prev:Option[Entity],next:Option[Entity]) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    hovered = next
  )



case class SelectMsg(prev:Seq[Part],next:Seq[Part]) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    selected = next
  )

case class ModMsg(prev:Set[KeyModifier],next:Set[KeyModifier]) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    modifiers = next
  )

