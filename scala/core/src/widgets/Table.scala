package semagrams.widgets

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import upickle.default._

import semagrams._
import semagrams.acsets._
import semagrams.acsets.Graphs._
import semagrams.util.Complex
import scala.annotation.targetName
import semagrams.util.msgError
import scala.util._


def noquotes(a:Any) = a
def noquotes(s:String) = if s.length > 1 & s.head == s.last & s.head == '"'
  then s.slice(1,s.length-1)
  else s 



def tableCell[T:ReadWriter](tSig:Signal[Option[T]]) = td(
  cls("dcell"),
  child.text <-- tSig.map(_ match
    case Some(v) => noquotes(write(v))
    case None => ""
  ),
)


def tableInput[T:ReadWriter](tSig:Signal[Option[T]],tObs:Observer[T]) = input(
  cls("dcell-edit"),
  value <-- tSig.map(_ match
    case Some(t) => noquotes(write(t))
    case None => ""
  ),
  placeholder("label?"),
  onMountFocus,
  onKeyPress
    .filter(_.keyCode == dom.KeyCode.Enter)
    .mapToValue
    .map(readStr[T])
    .collect {
      case Some(t) => t
    } --> tObs
)

/** Helper method with special handling for Value = String */ 
def readStr[T:ReadWriter](s:String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    case Failure(_) =>
      Try(read[T](write(s))) match
        case Success(t) => Some(t)
        case Failure(_) => None



case class PropTable(ob:Ob,cols:Seq[Property],keys:Seq[Property] = Seq()):

  // def edit(part:Part,prop:Property) = rowSig.map(rows =>
  //   rows.find(_.part == part) match
  //     case Some(row) => row.edit(prop)
  //     case None => 
  // )
  
  

  def laminarElt(
    modelSig: Signal[ACSet],
    messenger: Observer[Message[ACSet]]
  ): Element = 
    val rowSig = modelSig
      .map(_.parts(ROOT,ob))
      .split(pair => pair._1)(
        (part,pair,pairSig) => 
          PropRow(part,cols).laminarElt(
            pairSig.map(_._2.props),
            messenger
          )
      )

    table(
      headerRow,
      children <-- rowSig
    )

  def headerRow = th(ob.toString()) +: cols.map(prop => th(prop.toString()))



case class PropRow(part:Part,cols:Seq[Property]):

  // def edit(col:Property) = cols.indexOf(col) match
  //   case neg if neg < 0 => ()
  //   case i => cells.map(cellSeq =>
  //     cellSeq(i).edit()  
  //   )
  
  
  


  
  def laminarElt(
    propSig: Signal[PropMap],
    messenger: Observer[Message[ACSet]]
  ) = 

    val cellSig = propSig
      .combineWith(Signal.fromValue(cols))
      .map( (props,cols) => cols.map(
          col => (col,props.get(col))
      ))
      .split(pair => pair._1)(
        (col,initpair,pairSig) =>
          val cell = PropCell(part,col)
          
          cell.laminarElt(
            pairSig.map(pair =>
              val p = pair._1
              pair._2.asInstanceOf[Option[cell.prop.Value]]
            ),
            messenger.contramap {
              case v:col.Value =>
                SetSubpartMsg(part,col)(v)
            }
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

    )



//   def row(part:Part,propSig:Signal[PropMap],messenger:Observer[Message[ACSet]]) = 
//     val cells = cols.map(col => col.laminarCell(
//       part,
//       propSig.map(_.get(col)),
//       messenger.contramap(v => SetSubpartMsg(part,col)(v)))
//     )

//     tr(
//       td(part.path.head.toString()),
//       cells,
//       backgroundColor <-- propSig.map(_.get(Hovered) match
//         case Some(_) => "lightblue"
//         case None => "white"
//       ),
//       onMouseEnter.mapTo(SetSubpartMsg(part,Hovered)(())) --> messenger,
//       onMouseLeave.mapTo(RemoveSubpartMsg(part,Hovered)) --> messenger,

//     )

  

case class PropCell(part:Part,prop:Property):
  implicit val serializer: ReadWriter[prop.Value] = prop.rw

  val editVar = Var(false)

  

  def laminarElt(
    valSig: Signal[Option[prop.Value]],
    valObs: Observer[prop.Value]
  ) = td(
    cls := "cellTop",
    onDblClick.filter(_ => !editVar.now()).mapTo(true) --> editVar.writer,
    child <-- editVar.signal.map {b =>
      b match
        case true => tableInput(
          valSig,
          valObs
        )
        case false => tableCell(valSig)
    },
    onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
      .mapTo(false) --> editVar.writer,
  )
  
  def edit() = if !editVar.now()
    then editVar.set(true)
    else ()

