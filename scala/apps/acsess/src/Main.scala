package semagrams.graph

import semagrams._
import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._

// import Graphs._

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import upickle.default._




case object SchSchema extends Schema {

  val obs = Seq(SchemaOb.values*)
  val homs = Seq(SchemaHom.values*)
  val attrs = Seq(SchemaAttr.values*)

  enum SchemaOb extends Ob:
    override lazy val schema = SchEmpty
    case AttrType, Table, FKey, Attr, Comp, Input
  import SchemaOb._

  enum SchemaHom(val dom: SchemaOb, val codom: SchemaOb) extends Hom:
    val doms = Seq(PartType(Seq(dom)))
    val codoms = Seq(PartType(Seq(codom)))
    
    case FKeySrc extends SchemaHom(FKey,Table)
    case FKeyTgt extends SchemaHom(FKey,Table)

    case AttrSrc extends SchemaHom(Attr,Table)
    case AttrTgt extends SchemaHom(Attr,AttrType)

    case CompTgt extends SchemaHom(Comp,AttrType)
    case CompInput extends SchemaHom(Input,Comp)
    case InputType extends SchemaHom(Input,AttrType)


  enum SchemaAttr(val _doms: Seq[SchemaOb]) extends Attr:
    val doms = _doms.map(x => PartType(Seq(x)))

    case Name extends SchemaAttr(Seq(Table,AttrType,FKey,Attr,Comp,Input)) with PValue[String]


}

import SchSchema._
import SchemaOb._
import SchemaHom._
import SchemaAttr._





object AcsessDisplay extends ACSemagram:
  type Model = ACSet

  def layout(g: ACSet) = assignBends(Map(
    FKey -> (FKeySrc, FKeyTgt),
    Attr -> (AttrSrc, AttrTgt),
    // Comp -> ()
  ), 0.5)(g)

  val entitySources = Seq(
    ACSetEntitySource(Table, Rect()),
    ACSetEntitySource(AttrType, Disc(Name)),
    ACSetEdgeSource(FKey, FKeySrc, FKeyTgt, Arrow()),
    ACSetEdgeSource(Attr, AttrSrc, AttrTgt, Arrow()),
  )

  val schema: Schema = SchSchema


val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(Table)),
  Binding(KeyDownHook("A"), AddAtMouse(AttrType)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(
    ClickOnPartHook(MouseButton.Left, Set(KeyModifier.Shift))
      .filter(_.headOb == Table), 
    AddEdgeViaDrag(FKey, FKeySrc, FKeyTgt)
  ),
  Binding(
    ClickOnPartHook(MouseButton.Left)
      .filter(part => Seq(Table,AttrType).contains(part.headOb))
      , 
    MoveViaDrag()
  ),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),PartCallback(
    ent => ent match
      case p:Part => p.ty.path.head match
        case Table => tableTable.edit(p,Name)
        case AttrType => typeTable.edit(p,Name)
        case FKey => fkeyTable.edit(p,Name)
        case Attr => attrTable.edit(p,Name)
  )),
  Binding(KeyDownHook("?"), PrintModel)
)

val sema: SemagramElt = AcsessDisplay(bindings)

val semaAttrs = Seq(
  backgroundColor := "lightblue",
  height := "400px",
  width := "100%",
  border := "black",
  borderStyle := "solid",
  backgroundColor := "white",
  boxSizing := "border-box",
)

val messenger = Observer(
  (m:Message[ACSet]) =>
    sema.update(a => m.execute(a))
)


val tableTable = sema.propTable(Table,Seq(Name,Center,Fill))
val typeTable = sema.propTable(AttrType,Seq(Name,Center,Fill))
val fkeyTable = sema.propTable(FKey,Seq(Name,FKeySrc,FKeyTgt))
val attrTable = sema.propTable(Attr,Seq(Name,AttrSrc,AttrTgt))



object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaAttrs,
        ),
        tableTable.laminarElt(sema.signal,messenger),
        typeTable.laminarElt(sema.signal,messenger),
        fkeyTable.laminarElt(sema.signal,messenger),
        attrTable.laminarElt(sema.signal,messenger),
      )
        
      render(mountInto, mainDiv)
    }
  }
}
