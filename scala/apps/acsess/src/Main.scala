package semagrams.graph

import semagrams._
import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.simpleacsets.{Table => SimpleTable,Part => SimplePart,makeId}

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import upickle.default._
import semagrams.acsets.Graphs.GraphDisplay
import semagrams.sprites.ShapeProp
import semagrams.sprites.ShapeNode
import semagrams.sprites.ShapeOption
import semagrams.simpleacsets.SimpleSchema


object Schemas:
  // val Seq(T,V,F,A) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
  val Seq(t,v,f,a) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
  val tables = Seq(t,v,f,a)

  val Seq(fsrc,ftgt,asrc,atgt) = Seq(
    ("fkeySrc",f,t),
    ("fkeyTgt",f,t),
    ("attrSrc",a,t),
    ("attrTgt",a,v)
  ).map(simpleacsets.FKey.apply)
  val fkeys = Seq(fsrc,ftgt,asrc,atgt)

  val valTypes = Seq(
    simpleacsets.ValType[String]("Str")
  )
  val str = valTypes.head 

  val Seq(tname,vname,fname,aname) = Seq(
    ("tableName",t,str),
    ("typeName",v,str),
    ("fkeyName",f,str),
    ("attrName",a,str)
  ).map(simpleacsets.Attr.apply)
  val attrs = Seq(tname,vname,fname,aname)
  val elts = tables ++ fkeys

  val S = SimpleSchema(
    tables ++ fkeys ++ attrs:_*
  )

val schSchema = Schemas.S


// case object SchSchema extends Schema {

//   val obs = Seq(SchemaOb.values*)
//   val homs = Seq(SchemaHom.values*)
//   val attrs = Seq(SchemaAttr.values*)

//   enum SchemaOb extends Ob:
//     override lazy val schema = SchEmpty
//     case AttrType, Table, FKey, Attr, Comp, Input
//   import SchemaOb._

//   enum SchemaHom(val dom: SchemaOb, val codom: SchemaOb) extends Hom:
//     val doms = Seq(PartType(Seq(dom)))
//     val codoms = Seq(PartType(Seq(codom)))
    
//     case Schemas.fsrc extends SchemaHom(FKey,Table)
//     case FKeyTgt extends SchemaHom(FKey,Table)

//     case AttrSrc extends SchemaHom(Attr,Table)
//     case AttrTgt extends SchemaHom(Attr,AttrType)

//     case CompTgt extends SchemaHom(Comp,AttrType)
//     case CompInput extends SchemaHom(Input,Comp)
//     case InputType extends SchemaHom(Input,AttrType)
//   import SchemaHom._

//   enum SchemaAttr(val _doms: Seq[SchemaOb]) extends Attr:
//     val doms = _doms.map(x => PartType(Seq(x)))

//     case Name extends SchemaAttr(Seq(Table,AttrType,FKey,Attr,Comp,Input)) with PValue[String]
//   import SchemaAttr._

//   def toSimpleSchema(sch:ACSet): SimpleSchema = if sch.schema != this
//     then throw msgError(s"Bad schema read $sch:${sch.schema} != ${this}")
//     else 
//       val obMap = sch.parts(ROOT,Table).map((part,acset) =>
//         part -> SimpleTable(acset.props.get(Name) )
//       ).toMap

//       val fkeys = sch.parts(ROOT,FKey).map( (part,acset) =>
//         val fname = acset.props.get(Name).getOrElse(makeId("FKey"))
//         val src = acset.props.get(FKeySrc) match
//           case Some(part) => obMap(part)
//           case None => throw msgError(
//             s"Missing FKeySrc or Name for $part in ${sch.partsOnly(ROOT,Table)}"
//           )
        
//         val tgt = acset.props.get(FKeyTgt) match
//           case Some(part) => obMap(part)
//           case None => throw msgError(
//             s"Missing FKeyTgt for $part in ${sch.partsOnly(ROOT,Table)}"
//           )
//         simpleacsets.FKey(fname,src,tgt)
//       )
//       SimpleSchema(
//         obMap.values.toSeq ++ fkeys:_*
//       )

//       // SimpleSchema(
//       //   sch.partsOnly(Table),
//       //   sch.partsOnly(FKey),
//       //   sch.partsOnly(Attr)
//       // )
// }

// import SchSchema._
// import SchemaOb._
// import SchemaHom._
// import SchemaAttr._


// val (Table,ValType,FKey,Attr) = (SchemaStuff.t,SchemaStuff.v,SchemaStuff.f,SchemaStuff.a)


val schemaDisplay = GraphDisplay(
  schSchema,  
  Seq(
    Schemas.t -> ShapeNode(Schemas.tname,PropMap(Schemas.tname -> "Hi")),
    Schemas.v -> (ShapeNode(Schemas.vname),PropMap(ShapeProp.Shape -> ShapeOption.DiscShape,Fill -> "blue")),
    Schemas.f -> (Schemas.fsrc,Schemas.ftgt,Arrow(Schemas.fname)),
    Schemas.a -> (Schemas.asrc,Schemas.atgt,Arrow(Schemas.aname),PropMap() + (Stroke,"purple")),
  )
)


// object AcsessDisplay extends ACSemagram:
 
//   def layout(g: ACSet) = assignBends(Map(
//     FKey -> (FKeySrc, FKeyTgt),
//     Attr -> (AttrSrc, AttrTgt),
//     // Comp -> ()
//   ), 0.5)(g)

//   val entitySources = Seq(
//     ACSetEntitySource(Table, Rect(Name)),
//     ACSetEntitySource(AttrType, Disc(Name)),
//     ACSetEdgeSource(FKey, FKeySrc, FKeyTgt, Arrow(Name)),
//     ACSetEdgeSource(Attr, AttrSrc, AttrTgt, Arrow(Name)),
//   )

//   val schema: Schema = SchSchema
import MouseButton._
import KeyModifier._
import scala.language.implicitConversions

val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(Schemas.t)),
  Binding(KeyDownHook("A"), AddAtMouse(Schemas.v)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(
    ClickOnPartHook(Schemas.t,Left,Shift),
    AddEdgeViaDrag(
      Schemas.t -> (Schemas.f,Schemas.fsrc,Schemas.ftgt),
      Schemas.v -> (Schemas.a,Schemas.asrc,Schemas.atgt)
    )
  ),
  Binding(
    ClickOnPartHook(MouseButton.Left).filter(Schemas.t,Schemas.v), 
    MoveViaDrag()
  ),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),PartCallback(
    ent => ent match
      case p:Part => p.ty.path.head match
        case Schemas.t => tableTable.edit(p,Schemas.tname)
        case Schemas.v => typeTable.edit(p,Schemas.vname)
        case Schemas.f => fkeyTable.edit(p,Schemas.fname)
        case Schemas.a => attrTable.edit(p,Schemas.aname)
      case _ => ()
  )),
  Binding(KeyDownHook("?"), PrintModel),
)

// val sema: SemagramElt = AcsessDisplay(bindings)
val sema: SemagramElt = schemaDisplay(bindings)



val messenger = Observer(
  (m:Message[ACSet]) =>
    sema.update(a => m.execute(a))
)


val tableTable = sema.propTable(Schemas.t,Seq(Schemas.tname,Content,Center,Fill,ShapeProp.Shape,ImageURL))
val typeTable = sema.propTable(Schemas.v,Seq(Schemas.vname,Center,Fill,ShapeProp.Shape,ImageURL))
val fkeyTable = sema.propTable(Schemas.f,Seq(Schemas.fname,Schemas.fsrc,Schemas.ftgt))
val attrTable = sema.propTable(Schemas.a,Seq(Schemas.aname,Schemas.asrc,Schemas.atgt))

val semaProps = Seq(
  cls := "semaElt",
  // backgroundColor := "white"
)

object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaProps,
        ),
        tableTable.laminarElt(sema.signal,messenger),
        typeTable.laminarElt(sema.signal,messenger),
        fkeyTable.laminarElt(sema.signal,messenger),
        attrTable.laminarElt(sema.signal,messenger),
        onClick --> Observer(_ => sema.readout())
      )
        
      render(mountInto, mainDiv)
    }
  }
}
