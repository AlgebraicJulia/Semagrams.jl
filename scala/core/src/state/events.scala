package semagrams.state

import semagrams.util.Complex
import com.raquo.laminar.api.L._
import semagrams.Entity

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
  case MouseEnter(ent: Entity)
  case MouseLeave(ent: Entity)
  case MouseDown(ent: Option[Entity], button: MouseButton)
  case MouseUp(ent: Option[Entity], button: MouseButton)
  case Click(ent: Option[Entity], button: MouseButton)
  case DoubleClick(ent: Option[Entity], button: MouseButton)
  case MouseLeaveBox(pos: Complex)
  case MouseMove(pos: Complex)
  case KeyDown(key: String)
  case KeyUp(key: String)
  case ContextMenu(ent: Option[Entity])
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
