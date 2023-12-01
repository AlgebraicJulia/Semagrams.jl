package semagrams.state

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.partprops._

sealed trait Message[Model]:
  def execute(m: Model): Model
  val msgs: Seq[Message[Model]]
  def *(that: Message[Model]) = Message(this.msgs ++ that.msgs: _*)

object Message:
  def apply[Model](msgs: Message[Model]*): Message[Model] = msgs match
    case Seq(msg) => msg
    case _        => MsgSeq(msgs)

trait AtomicMessage[Model] extends Message[Model]:
  def execute(m: Model): Model
  val msgs = Seq(this)

case class FreeMsg[Model](exec: Model => Model) extends AtomicMessage[Model]:
  def execute(m: Model) = exec(m)

case class MsgSeq[A](msgs: Seq[Message[A]]) extends Message[A]:
  def execute(a: A) = msgs.foldLeft(a)((acset, msg) => msg.execute(acset))

object MsgSeq:
  def apply[A]() = new MsgSeq[A](Seq())

/* ACSet Messages */

sealed trait ACSetMsg[D: PartData] extends AtomicMessage[ACSet[D]]

case class AddPartMsg[D: PartData](ob: Ob, data: D, idOpt: Option[UID] = None)
    extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = idOpt match
    case Some(id) => a.addPartById(ob, id, data)._1
    case None     => a.addPart(ob, data)._1

object AddPartMsg:
  def apply[D: PartData](ob: Ob, props: PropMap, id: UID) =
    new AddPartMsg[D](ob, PartData(props), Some(id))

case class RemovePartMsg[D: PartData](part: Part) extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = a.remPart(part)

case class ChangePropMsg[D: PartData](part: Part, pval: PropChange[_])
    extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = pval match
    case PropChange(f, _, Some(v)) => a.setProp(f, part, v)
    case PropChange(f, _, None)    => a.remProp(f, part)

object ChangePropMsg:
  def apply[D: PartData](
      part: Part,
      f: Property,
      oldVal: f.Value,
      newVal: f.Value
  ) = new ChangePropMsg(part, PropChange(f, oldVal, newVal))

case class SetPropsMsg[D: PartData](part: Part, props: PropMap)
    extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = a.setProps(part, props)

case class SetPropMsg[D: PartData](part: Part, pval: PropVal[_])
    extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = pval match
    case PropVal(f, Some(v)) => a.setProp(f, part, v)
    case PropVal(f, None)    => a.remProp(f, part)

object SetPropMsg:
  def apply[D: PartData](part: Part, f: Property, newVal: f.Value) =
    new SetPropMsg(part, PropVal(f, newVal))

case class RemovePropMsg[D: PartData](prop: Property, part: Part)
    extends ACSetMsg[D]:
  def execute(a: ACSet[D]) = a.remProp(prop, part)
