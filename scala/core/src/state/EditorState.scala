package semagrams.state

import semagrams._
import semagrams.util._


import com.raquo.laminar.api._

import org.scalajs.dom

trait Enti


case class EditorState(
  hovered: Option[Entity],
  dims: Complex,
  mousePos: Complex,
  selected: Seq[Part],
  modifiers: Set[KeyModifier]
) {
  import KeyModifier._
  import MouseButton._

  def hoveredPart: Option[Part] = hovered match
    case Some(p:Part) => Some(p)
    case _ => None

  def isHovered: Boolean = hovered.isDefined
  

  def eventMsg(evt:Event): Message[EditorState] = evt match
    /* Mouse events */
    case MouseEnter(ent) => HoverMsg(hovered,Some(ent))
    case MouseLeave(ent) => HoverMsg(hovered, 
      if ent == Background then None else Some(Background)
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
    case MouseDown(Some(part:Part),Left) if modifiers.contains(Ctrl) => 
      if selected.contains(part)
      then SelectMsg(selected,selected.filter(_ != part))
      else SelectMsg(selected,(selected :+ part).distinct)
    case MouseDown(Some(Background),Left) if selected.nonEmpty =>
      SelectMsg(selected,Seq())
    case MouseDown(Some(part:Part),Left) =>
      SelectMsg(selected,Seq(part))
 
    case _ => Message()
  

  def processEvent(evt: Event): EditorState = 
    eventMsg(evt).execute(this)


}

object EditorState {

  def apply() = new EditorState(None,Complex(1,1),Complex(0,0),Seq(),Set())


  def listen(into: L.Observer[Event]) = {
    dom.document.addEventListener("keydown",(ev:dom.KeyboardEvent) => into.onNext(KeyDown(ev.key)))
    dom.document.addEventListener("keyup",(ev:dom.KeyboardEvent) => into.onNext(KeyUp(ev.key)))
    dom.window.addEventListener("blur",_ => into.onNext(Blur()))
  }

}


sealed trait EditorMsg extends AtomicMessage[EditorState]

case class ResizeMsg(z:Complex) extends EditorMsg:
  def execute(es:EditorState) = es.copy(
    dims = z
  )

case class HoverMsg(prev:Option[Entity]|Option[Part],next:Option[Entity]|Option[Part]) extends EditorMsg:
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

