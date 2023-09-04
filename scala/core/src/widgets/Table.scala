package semagrams.widgets

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom

import upickle.default._

import semagrams._
import semagrams.acsets._
import semagrams.acsets.Graphs._



def tableCell[T:ReadWriter](tSig:Signal[Option[T]]) = td(
  cls("dcell"),
  child.text <-- tSig.map(_ match
    case Some(v) => write(v)
    case None => ""
  )
)

def tableInput[T:ReadWriter](tSig:Signal[Option[T]],tObs:Observer[T]) = input(
  cls("dcell-edit"),
  value <-- tSig.map(_ match
    case Some(t) => write(t)
    case None => ""
  ),
  placeholder("label?"),
  onMountFocus,
  onKeyPress
    .filter(_.keyCode == dom.KeyCode.Enter)
    .mapToValue
    .map(s => {
      println(s"got here? $s")
      val ret = read[T](write(s))
      println("here?") 
      ret
    })
    --> tObs
)
  
def propCell(prop:Property)(tSig:Signal[Option[prop.Value]],tObs:Observer[prop.Value] = Observer(t => ())) =
  val editVar = Var(false)
  td(
    cls := "cellTop",
    onDblClick.filter(_ => !editVar.now()).mapTo(true) --> editVar.writer,
    child <-- editVar.signal.map[HtmlElement] {
      case true => tableInput(tSig,tObs)(prop.rw)
      case false => tableCell(tSig)(prop.rw)
    },
    onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
      .mapTo(false) --> editVar.writer
  )




case class PropTable(cols:Seq[Property],keys:Seq[Property] = Seq()) {
  
  def headerRow() = tr(
    th("ID"),
    cols.map(col => th(col.toString()))
  )

  def row(part:Part,propSig:Signal[PropMap],messenger:Observer[Message[ACSet]]) = 
    val cells = cols.map(col => col.laminarCell(
      propSig.map(_.get(col)),
      messenger.contramap(v => SetSubpartMsg(part,col)(v)))
    )
    
    tr(
      td(part.path.head.toString()),
      cells
    )
    
  

  def rows(ob:Ob,modelSig:Signal[ACSet],messenger:Observer[Message[ACSet]]) = 
    modelSig.map(acset =>
      acset.parts(ROOT,ob)
    ).split(_._1){
      case (_,(part,acset),pairSig) =>
        row(part,pairSig.map(_._2.props),messenger)
    }


  def laminarElt(ob:Ob,tableSig:Signal[ACSet],messenger:Observer[Message[ACSet]]) = table(
    headerRow(),
    children <-- rows(V,tableSig,messenger)
  )

}


