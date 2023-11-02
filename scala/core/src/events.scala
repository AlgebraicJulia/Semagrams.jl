package semagrams

import semagrams.util.Complex
import com.raquo.laminar.api.L._
import semagrams.acsets.abstr.Part

enum MouseButton:
  case Left
  case Middle
  case Right

object MouseButton {
  def fromJS(idx: Int) = idx match {
    case 0 => Left
    case 1 => Middle
    case 2 => Right
  }
}

enum Event {
  case MouseEnter(ent: Part)
  case MouseLeave(ent: Part)
  case MouseDown(ent: Option[Part], button: MouseButton)
  case MouseUp(ent: Option[Part], button: MouseButton)
  case Click(ent: Option[Part], button: MouseButton)
  case DoubleClick(ent: Option[Part], button: MouseButton)
  case MouseLeaveBox(pos: Complex)
  case MouseMove(pos: Complex)
  case KeyDown(key: String)
  case KeyUp(key: String)
  case ContextMenu(ent: Option[Part])
  case Resize(size:Complex)
  case MsgEvent[Model](msg:Message[Model])
  case Blur()
}


sealed trait Message[Model]:
  def execute(m:Model): Model
  val msgs: Seq[Message[Model]]
  def *(that:Message[Model]) = Message(this.msgs ++ that.msgs:_*)

object Message:
  def apply[Model](msgs:Message[Model]*): Message[Model] = msgs match
    case Seq(msg) => msg
    case _ => MsgSeq(msgs)
   


trait AtomicMessage[Model] extends Message[Model]:
  def execute(m:Model): Model
  val msgs = Seq(this)

case class FreeMsg[Model](exec:Model => Model) extends AtomicMessage[Model]:
  def execute(m:Model) = exec(m)

case class MsgSeq[A](msgs:Seq[Message[A]]) extends Message[A]:
  def execute(a:A) = msgs.foldLeft(a)((acset,msg) => msg.execute(acset))
  
object MsgSeq:
  def apply[A]() = new MsgSeq[A](Seq())







export Event._
