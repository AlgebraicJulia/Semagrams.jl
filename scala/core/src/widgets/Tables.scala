package semagrams.widgets

import com.raquo.laminar.api.L._
import org.scalajs.dom

import upickle.default._

import semagrams._
import semagrams.acsets._
import scala.util._




/** Edit model 
 * 
 *  The editing state Option[(Part,Property)] is stored in the
 *  table, and the cells pass messages to modify it. Some messages
 *  must be converted into simpler terms before reaching the top
 *  level. 
 * **/


type Editing = Option[(Part,Property)]

/** A trait for messages to toggle editable inputs **/
sealed trait EditMsg

/** A subtrait for "global" messages that can be interpreted
 *  without further information at the top level
 */
sealed trait TableEditMsg extends EditMsg with Message[Editing]
case class SetEditMsg(part:Part,prop:Property) extends TableEditMsg:
  def execute(e:Editing) = Some((part,prop))

case class ClearEditMsg() extends TableEditMsg:
  def execute(e:Editing) = None

/** A subtrait for "local" messages that must be transformed before
 *  reaching the top level.
 */
sealed trait CellEditMsg extends EditMsg
case class EditNextCellMsg(part:Part,prop:Property) extends CellEditMsg
case class EditPrevCellMsg(part:Part,prop:Property) extends CellEditMsg





/** A case class to generate laminar tables from ACSets **/ 
case class PropTable(ob:Ob,cols:Seq[Property],keys:Seq[Property]):

  val editingVar: Var[Option[(Part,Property)]] = Var(None)

  /** External callback to modify the editing state **/
  def edit(part:Entity,prop:Property) = part match
    case part:Part => editObs.onNext(SetEditMsg(part,prop))
    case _ => ()


  def headerRow = tr(
    th(ob.toString()) +: cols.map(prop => th(prop.toString()))
  )

  /** Observer to update the editing variable. **/
  val editObs: Observer[TableEditMsg] = editingVar.updater {
    (e,msg) => msg match
      case SetEditMsg(part,prop) => Some((part,prop))
      case ClearEditMsg() => None
  }

  /** Create an editable laminar element. */ 
  def laminarElt(modelSig:Signal[ACSet],messenger:Observer[Message[ACSet]]) =

    case class RowData(part:Part,props:PropMap,prev:Option[Part],next:Option[Part])

    /* Collect the props and neighbors of each part */
    val rowData: Signal[Seq[RowData]] = modelSig.map{ acset =>
      val (parts,acsets) = acset.parts(ROOT,ob).unzip
      parts.zip(
        acsets.map(_.props)
      ).map( (part,props) =>
        val colProps = props.filterKeys(prop => cols.contains(prop))
        val (prv,nxt) = neighbors(part,parts)
        RowData(part,colProps,prv,nxt)  
      )
    }
    
    /* Create row element signals */
    val rowElts = rowData.split(_.part) { case (part,data,dataSig) =>
      PropRow(part,cols).laminarElt(
        dataSig.map(data => (data.props,data.prev,data.next)),               
        messenger,
        editingVar.signal.map(partPropOpt =>
          partPropOpt
            .filter{ case (pt,_) => pt == part }
            .map(_._2)
        ),
        editObs
      )
    }

    /* Create table element. `height` attribute full-size divs in table */
    table(
      headerRow,
      children <-- rowElts,
      height := "100%"
    )



object PropTable:

/** Convenience method to deduplicate and move keys to the front */
  def apply(ob:Ob,cols:Seq[Property],keys:Seq[Property] = Seq()) = 
    keys match
      case Seq() => 
        new PropTable(ob,cols.distinct,cols.distinct)
      case _ =>
        val ks = keys.distinct
        val cs = ks ++ cols.distinct.filterNot(ks.contains)
        new PropTable(ob,cs,ks)



/** A case class to generate table row elements from `PropMap`s **/ 
case class PropRow(part:Part,cols:Seq[Property]):

  /** Converts `CellEditMsg` into `TableEditMsg` */
  def modifyObs(editObs:Observer[TableEditMsg],prv:Option[Part],nxt:Option[Part]): Observer[EditMsg] = 
    editObs.contramap[EditMsg] {
      case msg:TableEditMsg => msg
      case EditNextCellMsg(part, prop) => 
        (next(prop,cols),nxt) match
          case (Some(nxtprop),_) => SetEditMsg(part,nxtprop)
          case (_,Some(nxtpart)) => SetEditMsg(nxtpart,cols.head)
          case _ => ClearEditMsg()
      case EditPrevCellMsg(part, prop) => 
        (prev(prop,cols),prv) match
          case (Some(prvprop),_) => SetEditMsg(part,prvprop)
          case (_,Some(prvpart)) => SetEditMsg(prvpart,cols.last)
          case _ => ClearEditMsg()
  }

  /** Create an editable laminar element. */ 
  def laminarElt(
    rowSig:Signal[(PropMap,Option[Part],Option[Part])],
    messenger:Observer[Message[ACSet]],
    editSig:Signal[Option[Property]],
    editObs:Observer[TableEditMsg],
  ) =

    /* Create modifiers adding a cell for each column property. */
    val cellMods = cols.map { col => 
      val cellSig: Signal[Element] = rowSig.splitOne{
        case (props,prv,nxt) => (props.get(col),prv,nxt)
      }{
        case ((pval,prv,nxt),tuple,tupleSig) =>
          val cell = PropCell[col.Value](part,col)(pval)
          
          cell.laminarElt(
            tupleSig.map(_._1.get(col)),        // Signal[PropMap]
            messenger,                          // Observer[Message[ACSet]]
            editSig.map(_.contains(col)),       // Signal[Boolean]
            modifyObs(editObs,prv,nxt)          // Observer[EditMsg]
          ) 
      }

      /* Return a modifier */
      child <-- cellSig 
    }

    /* Return a row element */
    tr(
      td(part.headId.id.toString()),
      cellMods,
      backgroundColor <-- rowSig.map(_._1.get(Hovered) match
        case Some(_) => "lightblue"
        case None => "white"
      ),
      onMouseEnter.mapTo(SetSubpartMsg(part,Hovered)(())) --> messenger,
      onMouseLeave.mapTo(RemoveSubpartMsg(part,Hovered)) --> messenger,
    )


/** A case class to generate table cells from `Property`s **/ 
case class PropCell[T](part:Part,prop:Property {type Value = T})(tOpt:Option[T]):
  
  
  def laminarElt(
    valSig: Signal[Option[prop.Value]],
    messenger: Observer[Message[ACSet]],
    editSig: Signal[Boolean],
    editObs: Observer[EditMsg]
  ) = 
    
    val inputBus = EventBus[(InputMsg,Option[prop.Value])]()

    def editMsg(inputMsg:InputMsg,vOpt:Option[prop.Value]): EditMsg = inputMsg match
      case EscMsg(_) | EnterMsg(_) => ClearEditMsg()
      case TabMsg(mods) => if mods.contains(KeyModifier.Shift)
        then EditPrevCellMsg(part,prop)
        else EditNextCellMsg(part,prop)
    
    def acsetMsg(inputMsg:InputMsg,vOpt:Option[prop.Value]): Option[Message[ACSet]] = 
      inputMsg match
        case EscMsg(_) => None
        case EnterMsg(_) | TabMsg(_) => vOpt match
          case Some(v) => Some(SetSubpartMsg(part,prop)(v))
          case None => Some(RemoveSubpartMsg(part,prop))
        
      
    
    td(
      cls := "dcell",
      inputBus.events.map(editMsg) --> editObs,
      inputBus.events.collectOpt(acsetMsg) --> messenger,
      child <-- editSig.map {b =>
        b match
          case true => tableInput[prop.Value](
            inputBus.writer,
            tOpt
          )(prop.rw)
          case false => tableCell(
            valSig,
            editObs.contramap(
              (_:Unit) => SetEditMsg(part,prop)
            )
          )(prop.rw)
      },
    )






/** Create a laminar table cell with a value signal and a input toggle on
 *  double click.
 */  
def tableCell[T:ReadWriter](tSig:Signal[Option[T]],toggle:Observer[Unit]) = div(
  cls("dcell-text"),
  child.text <-- tSig.map(_ match
    case Some(v) => noquotes(write(v))
    case None => ""
  ),
  onDblClick.mapTo(()) --> toggle,
  height := "100%"
)

val onEnter = onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
val onEsc = onKeyDown.filter(_.keyCode == dom.KeyCode.Escape)
val onTab = onKeyDown.filter(evt => evt.keyCode == dom.KeyCode.Tab).preventDefault


/** Triggers for input modification */
enum InputMsg(mods:Set[KeyModifier]):
  case TabMsg(mods:Set[KeyModifier]) extends InputMsg(mods)
  case EnterMsg(mods:Set[KeyModifier]) extends InputMsg(mods)
  case EscMsg(mods:Set[KeyModifier]) extends InputMsg(mods)
export InputMsg._

/** All combinations of key modifiers */
val modFilters = KeyModifier.values.toSet.subsets.toSeq

/** Test a keyboard event against a set of key modifiers */
def evtFilter(modOptions:Set[KeyModifier])(evt:dom.KeyboardEvent) =
  KeyModifier.values.forall(mod =>
    (modOptions.contains(mod) & mod.isSet(evt)) |
    (!modOptions.contains(mod) & ! mod.isSet(evt))
  )

/** Create a laminar table cell input with an initial value. The input observes
 *  pairs of `InputMsg` (key + mods) and an optional return value.
 */  
def tableInput[T:ReadWriter](inputObs:Observer[(InputMsg,Option[T])],init:Option[T] = None) = 
  td(cls("dcell"),
    input(cls("dcell-edit"),
      value := (init match
        case Some(t) => noquotes(write(t))
        case None => ""
      ),
      onKeyDown.stopPropagation --> Observer(_=> ()),
      onMountFocus,
      modFilters.flatMap(mods => 
        def catchEmpty(s:String) = (init,readStr[T](s)) match
          case (None,Some("")) => None
          case (_,opt) => opt
        
        Seq(
          onEnter.filter(evtFilter(mods))
            .mapToValue
            .map(v => (EnterMsg(mods),catchEmpty(v))) --> inputObs,  
          onTab.filter(evtFilter(mods))
            .mapToValue
            .map(v => (TabMsg(mods),catchEmpty(v))) --> inputObs,  
          onEsc.filter(evtFilter(mods))
            .mapToValue
            .map(v => (EscMsg(mods),catchEmpty(v))) --> inputObs,  
        ))
    )
  )

/** Utilities **/



/** Return `Some(next value)` in `ts` after `t` (or `None`) */
def next[T](t:T,ts:Seq[T]): Option[T] = 
  if ts.indexOf(t) + 1 < ts.length
  then 
    Some(ts(ts.indexOf(t) + 1))
  else
    None

/** Return `Some(prev value)` in `ts` before `t` (or `None`) */
def prev[T](t:T,ts:Seq[T]) = 
  if ts.indexOf(t) > 0
  then Some(ts(ts.indexOf(t) - 1))
  else None


/** Return (optional) neighbors of `t` in `ts` */
def neighbors[T](t:T,ts:Seq[T]) = (prev(t,ts),next(t,ts))



/** Helper method with special handling for Value = String */ 
def readStr[T:ReadWriter](s:String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    case Failure(_) =>
      Try(read[T](write(s))) match
        case Success(t) => Some(t)
        case Failure(_) => None


def noquotes(s:String) = if s.length > 1 & s.head == s.last & s.head == '"'
  then s.slice(1,s.length-1)
  else s 






