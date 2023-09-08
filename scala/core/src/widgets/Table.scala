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

val onTab = onKeyPress.filter(_.keyCode == dom.KeyCode.Tab)

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
  onEsc.mapTo(()) --> toggle,
)



case class EditState(part:Part,prop:Property,editing:Boolean)

case class PropCell(part:Part,prop:Property,
  editSig:Signal[Boolean],
  editToggle: Observer[EditState]
):
  implicit val serializer: ReadWriter[prop.Value] = prop.rw




  def laminarElt[T:ReadWriter](
    valSig: Signal[Option[T]],
    valObs: Observer[prop.Value],
    editObs: Observer[EditState]
  ) = td(
    cls := "dcell",
    child <-- editSig.map {b =>
      b match
        case true => tableInput(
          valSig,
          valObs.contramap(_.asInstanceOf[prop.Value]),
          editObs.contramap(
            (_:Unit) => EditState(part,prop,false)
          )
        )
        case false => tableCell(valSig,
          editObs.contramap((_:Unit) => EditState(part,prop,true))
        )
    },
    onEnter.mapTo(EditState(part,prop,false)) --> editObs,
    // onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
    //   .mapTo(false) --> editVar.writer,
  )

  // def edit() = if !editVar.now()
  //   then editVar.set(true)
  //   else ()




/** Helper method with special handling for Value = String */ 
def readStr[T:ReadWriter](s:String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    case Failure(_) =>
      Try(read[T](write(s))) match
        case Success(t) => Some(t)
        case Failure(_) => None




case class PropRow(part:Part,cols:Seq[Property],
  editSig:Signal[Set[Property]],
  editObs:Observer[EditState]
):

  // def edit(col:Property) = cols.indexOf(col) match
  //   case neg if neg < 0 => ()
  //   case i => cells.map(cellSeq =>
  //     cellSeq(i).edit()  
  //   )
  
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



case class PropTable(ob:Ob,cols:Seq[Property],keys:Seq[Property] = Seq()):

  // def edit(part:Part,prop:Property) = rowSig.map(rows =>
  //   rows.find(_.part == part) match
  //     case Some(row) => row.edit(prop)
  //     case None => 
  // )

  def edit(ent:Entity,prop:Property) = ent match
    case part:Part =>
      editObs.onNext(EditState(part,prop,true))
    case _ => ()



  val editsVar: Var[Map[Part,Set[Property]]] = 
    Var(Map().withDefault(_ => Set()))

  val editObs: Observer[EditState] = Observer {
    case EditState(part,prop,editing) => editsVar.update(
      edits => editing match
        case true => 
          edits + (part -> (edits(part) + prop))  
        case false => edits(part).toSeq match
          case Seq() => edits - part
          case Seq(p) if p==prop => edits - part
          case _ => edits + (part -> (edits(part) - prop)) 

    )


  // val edit = (part:Part,prop:Property) => println("Hi")


  }


  def laminarElt(
    modelSig: Signal[ACSet],
    messenger: Observer[Message[ACSet]]
  ): Element = 
    val rowSig = modelSig
      .map(_.parts(ROOT,ob))
      .split(pair => pair._1)(
        (part,pair,pairSig) => 
          PropRow(part,cols,
            editsVar.signal.map(_(part)),
            editObs
          ).laminarElt(
            pairSig.map(_._2.props),
            messenger
          )
      )

    table(
      headerRow,
      children <-- rowSig,
      height := "100%"
    )

  def headerRow = tr(
    th(ob.toString()) +: cols.map(prop => th(prop.toString()))
  )
