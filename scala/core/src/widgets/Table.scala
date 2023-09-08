package semagrams.widgets

import com.raquo.laminar.api.L._
import org.scalajs.dom

import upickle.default._

import semagrams._
import semagrams.acsets._
import scala.util._


def noquotes(a:Any) = a
def noquotes(s:String) = if s.length > 1 & s.head == s.last & s.head == '"'
  then s.slice(1,s.length-1)
  else s 



def tableCell[T:ReadWriter](tSig:Signal[Option[T]],toggle:Observer[Unit]) = div(
  cls("dcell-text"),
  child.text <-- tSig.map(_ match
    case Some(v) => noquotes(write(v))
    case None => ""
  ),
  onDblClick.mapTo(()) --> toggle,
  height := "100%",
)

val onEnter = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
val onEsc = onKeyDown.filter(_.keyCode == dom.KeyCode.Escape)

val onTab = onKeyDown.filter(evt => evt.keyCode == dom.KeyCode.Tab)



def tableInput[T:ReadWriter](tSig:Signal[Option[T]],tObs:Observer[T],toggle:Observer[Unit]) = input(
  cls("dcell-edit"),
  value <-- tSig.map(_ match
    case Some(t) => noquotes(write(t))
    case None => ""
  ),
  placeholder("label?"),
  onMountFocus,
  onEnter.mapToValue
    .map(readStr[T])
    .collect {
      case Some(t) => t
    } --> tObs,
  onTab.mapToValue
    .map(readStr[T])
    .collect {
      case Some(t) => t
    } --> tObs,
  onEsc.mapTo(()) --> toggle,
)

case class PropCell(part:Part,prop:Property,
  editSig:Signal[Boolean],
  editToggle: Observer[EditMsg]
):
  implicit val serializer: ReadWriter[prop.Value] = prop.rw




  def laminarElt[T:ReadWriter](
    valSig: Signal[Option[T]],
    valObs: Observer[prop.Value],
    editObs: Observer[EditMsg]
  ) = td(
    cls := "dcell",
    child <-- editSig.map {b =>
      b match
        case true => tableInput(
          valSig,
          valObs.contramap(_.asInstanceOf[prop.Value]),
          editObs.contramap(
            (_:Unit) => SetEditMsg(part,prop,false)
          )
        )
        case false => tableCell(valSig,
          editObs.contramap((_:Unit) => SetEditMsg(part,prop,true))
        )
    },
    onEnter.mapTo(SetEditMsg(part,prop,false)) --> editObs,
    onTab.preventDefault
      .map( _.getModifierState("Shift") match
        case true => 
          // Shift + Tab
          EditPrevCellMsg(part,prop)
        case false => 
          // Tab
          EditNextCellMsg(part,prop)
      ) --> editObs
  )



/** Helper method with special handling for Value = String */ 
def readStr[T:ReadWriter](s:String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    case Failure(_) =>
      Try(read[T](write(s))) match
        case Success(t) => Some(t)
        case Failure(_) => None




case class PropRow(part:Part,cols:Seq[Property],
  neighbors:(Part,Part),
  editSig:Signal[Set[Property]],
  editObs:Observer[EditMsg]
):

  val cells = cols.map(col => PropCell(part,col,
    editSig.map(_.contains(col)),
    editObs
  ))
  


  
  def laminarElt(
    propSig: Signal[PropMap],
    messenger: Observer[Message[ACSet]]
  ) = 

    val cellSig = propSig
      .combineWith(Signal.fromValue(cells))
      .map { case (props,cells) => 
        cells.map( cell =>
          (cell,props.get(cell.prop))
        )
      }
      .split(pair => pair._1)(
        (cell,initpair,pairSig) => cell.laminarElt(
          pairSig.map(pair =>
            pair._2.asInstanceOf[Option[cell.prop.Value]]
          ),
          messenger.contramap(
            (v:cell.prop.Value) => 
              SetSubpartMsg(part,cell.prop)(v)
          ),
          editObs
        )
      )


    tr(
      td(part.path.head._2.id.toString()),
      children <-- cellSig,
      backgroundColor <-- propSig.map(_.get(Hovered) match
        case Some(_) => "lightblue"
        case None => "white"
      ),
      onMouseEnter.mapTo(SetSubpartMsg(part,Hovered)(())) --> messenger,
      onMouseLeave.mapTo(RemoveSubpartMsg(part,Hovered)) --> messenger,
      height := "100%",
    )






type Edits = Map[Part,Set[Property]]

sealed trait EditMsg:
  val part:Part
  val prop:Property

case class SetEditMsg(part:Part,prop:Property,editing:Boolean) extends EditMsg
case class EditNextCellMsg(part:Part,prop:Property) extends EditMsg
case class EditPrevCellMsg(part:Part,prop:Property) extends EditMsg

extension (edits:Edits) {
  def receive(msg:SetEditMsg): Edits = msg match
    case SetEditMsg(part,prop,editing) => editing match
      case true => 
        edits + (part -> (edits(part) + prop))  
      case false => edits(part).toSeq match
        case Seq() => edits - part
        case Seq(p) if p==prop => edits - part
        case _ => edits + (part -> (edits(part) - prop))

  def receiveAll(msgs:SetEditMsg*): Edits = msgs match
    case Seq() => edits
    case head +: tail => edits.receive(head).receiveAll(tail:_*)
}



case class PropTable(cols:Seq[Property],partsSig:Signal[Seq[(Part,ACSet)]],keys:Seq[Property] = Seq()):
  
  val editBus = new EventBus[EditMsg]
  val editsVar: Var[Edits] = 
    Var(Map().withDefault(_ => Set()))

  def next[T](t:T,ts:Seq[T]) = ts(
    (ts.indexOf(t) + 1) % ts.length
  )

  // Extra factor of `ts.length` avoids negative modulus
  def prev[T](t:T,ts:Seq[T]) = ts(
    (ts.indexOf(t) + ts.length - 1) % ts.length
  )

  def neighbors[T](t:T,ts:Seq[T]) = (prev(t,ts),next(t,ts))



  val editObs: Observer[EditMsg] = editsVar.updater {
    case (edits,msg) => msg match
      case msg:SetEditMsg => edits.receive(msg)
      case EditNextCellMsg(part, prop) => edits.receiveAll(
        SetEditMsg(part,prop,false),
        SetEditMsg(part,next(prop,cols),true)
      )
      case EditPrevCellMsg(part, prop) => edits.receiveAll(
        SetEditMsg(part,prop,false),
        SetEditMsg(part,prev(prop,cols),true)
      )
    
  }

  def setEdit(part:Entity,prop:Property,editing:Boolean = true) = part match
    case part:Part => editObs.onNext(SetEditMsg(part,prop,editing))
    case _ => ()
  
  def laminarElt(messenger: Observer[Message[ACSet]]): Element = 
    
    val triplesSig = partsSig.map {pairs => 
      pairs.unzip match {
        case (parts,acsets) => pairs.map {
          case (part,acset) => (part,acset,neighbors(part,parts))
        } 
      }
    }
    val rowSig = triplesSig.split(_._1){
      case (part,(_,propmap,neighbors),tripleSig) =>
        PropRow(part,cols,neighbors,
          editsVar.signal.map(_(part)),
          editObs
        ).laminarElt(
          tripleSig.map(_._2.props),
          messenger
        )
    }

    table(
      headerRow,
      children <-- rowSig,
      height := "100%"
    )

  def headerRow = tr(
    th("+") +: cols.map(prop => th(prop.toString()))
  )

object PropTable:
  def apply(cols:Seq[Property],parts:Signal[Seq[(Part,ACSet)]],keys:Seq[Property] = Seq()) = 
    keys match
      case Seq() => 
        new PropTable(cols.distinct,parts,cols.distinct)
      case _ =>
        val ks = keys.distinct
        val cs = ks ++ cols.distinct.filterNot(ks.contains)
        new PropTable(cs,parts,ks)


    