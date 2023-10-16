package semagrams.acsess

import semagrams._
import semagrams.api._
import semagrams.acsets.abstr._
import semagrams.graphs._
import semagrams.acsets.simple._
import semagrams.bindings._
// import semagrams.simpleacsets.{Table => SimpleTable,Part => SimplePart,makeId}

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import upickle.default._
// import semagrams.acsets.Graphs.GraphDisplay
import semagrams.sprites.ShapeProp
import semagrams.sprites.ShapeNode
import semagrams.sprites.ShapeOption
import semagrams.acsets.simple.SimpleACSet.simpleACSetIsACSet
import cats.effect.IO
// import semagrams.simpleacsets.SimpleSchema



implicit val schIsSchema: Schema[SimpleSchema] =
  simpleSchemaIsSchema
implicit val acsetIsACSet: ACSetWithSchAndData2[SimpleSchema,PropMap][SimpleACSet[SimpleSchema]] =
  simpleACSetIsACSet[SimpleSchema]




val a = UndoableVar(schSchema.toACSet)

type AA = SimpleACSet[SimpleSchema]


val schemaBindings = Seq(
  Binding(KeyDownHook("a"), AddAtMouse(tableOb)),
)


val schemaDisplay = GraphDisplay(
  schSchema,
  Seq(VertexDef(tableOb,Rect(Content),PropMap() + (Fill,"red") + (Content,"Hi"))),
  Seq()
  // Seq(EdgeDef(fkeyOb,fkeySrc,fkeyTgt,Arrow(Content),PropMap() + (Stroke,"purple")))
)

val schemaSema = schemaDisplay(schemaBindings,schSchema.toACSet)



val currentTable: Var[Option[Table]] = Var(Some(tableOb))


case class AddTableElt[A:ACSet]() extends Action[Unit, A] {
  def apply(_p: Unit, r: Action.Resources[A]) = 
    r.stateVar.now().hovered match
    case Some(ent) => 
      val pos = r.stateVar.now().mousePos match
        case Complex(0,0) => r.stateVar.now().dims/2.0
        case z => z
      currentTable.now() match
        case Some(table) =>      
          IO(r.modelVar.update(
            _.addPart(table, PropMap() + (Center -> pos))._1
          ))
        case None => IO(())
    
    case None => IO(())
  

  def description = s"add a new part of selected type at current mouse position"
}

val acsetBindings = Seq(
  Binding(
    KeyDownHook("b"),
    AddTableElt()  
  )
)

val acsetSemaSig = a.signal.combineWith(currentTable.signal)
  .map( (acset,table) =>

    val acsetDisplay = GraphDisplay(
      acset.schema,
      acset.schema.obs.map(table =>
        VertexDef(table,Rect(Content),PropMap() + (Fill,"blue")) , 
      ),
      Seq()
    )

    acsetDisplay(acsetBindings,acset)

) 





val semaAttrs = Seq(
  backgroundColor := "white",
  height := "400px",
  width := "100%",
  border := "black",
  borderStyle := "solid",
  backgroundColor := "white",
  boxSizing := "border-box",
)

val messenger = Observer(
  (m:Message[SimpleACSet[SimpleSchema]]) =>
    a.set(m.execute(a.now()))
)



object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        schemaSema.elt.amend(
          semaAttrs,
        ),
        div(
          "Hi",
          child <-- acsetSemaSig.map(_.elt.amend(
            semaAttrs,
          )),
        )

        // tableTable.laminarElt(sema.signal,messenger),
        // typeTable.laminarElt(sema.signal,messenger),
        // fkeyTable.laminarElt(sema.signal,messenger),
        // attrTable.laminarElt(sema.signal,messenger),
        // onClick --> Observer(_ => sema.readout())
      )
        
      render(mountInto, mainDiv)
    }
  }
}




// val schemaBindings = Seq[Binding[SimpleACSet[SimpleSchema]]](
//   Binding(KeyDownHook("a"), AddAtMouse(tableOb)),
//   Binding(
//     ClickOnPartHook(MouseButton.Left).filter(tableOb), 
//     MoveViaDrag()
//   ),
//   Binding(
//     ClickOnPartHook(MouseButton.Left, KeyModifier.Shift)
//       .filter(tableOb), 
//     AddEdgeViaDrag(fkeyOb, fkeySrc, fkeyTgt)
//   ),

//   Binding(KeyDownHook("A"), AddAtMouse(Schemas.v)),
//   Binding(KeyDownHook("d"), DeleteHovered()),
//   Binding(
//     ClickOnPartHook(Schemas.t,Left,Shift),
//     AddEdgeViaDrag(
//       Schemas.t -> (Schemas.f,Schemas.fsrc,Schemas.ftgt),
//       Schemas.v -> (Schemas.a,Schemas.asrc,Schemas.atgt)
//     )
//   ),
//   Binding(
//     ClickOnPartHook(MouseButton.Left).filter(Schemas.t,Schemas.v), 
//     MoveViaDrag()
//   ),
//   Binding(MsgHook(),ProcessMsg()),
//   Binding(DoubleClickOnPartHook(),PartCallback(
//     ent => ent match
//       case p:Part => p.ty.path.head match
//         case Schemas.t => tableTable.edit(p,Schemas.tname)
//         case Schemas.v => typeTable.edit(p,Schemas.vname)
//         case Schemas.f => fkeyTable.edit(p,Schemas.fname)
//         case Schemas.a => attrTable.edit(p,Schemas.aname)
//       case _ => ()
//   )),
//   Binding(KeyDownHook("?"), PrintModel()),
// )


// val acsetdisplaySig = a.signal.map(acset =>
//   GraphDisplay(
//     implicitly[ACSet[SimpleACSet[SimpleSchema]]](acset).sch,
//     s
//   )  
// )




// object Schemas:
//   // val Seq(T,V,F,A) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
//   val Seq(t,v,f,a) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
//   val tables = Seq(t,v,f,a)

//   val Seq(fsrc,ftgt,asrc,atgt) = Seq(
//     ("fkeySrc",f,t),
//     ("fkeyTgt",f,t),
//     ("attrSrc",a,t),
//     ("attrTgt",a,v)
//   ).map(simpleacsets.FKey.apply)
//   val fkeys = Seq(fsrc,ftgt,asrc,atgt)

//   val valTypes = Seq(
//     simpleacsets.ValType[String]("Str")
//   )
//   val str = valTypes.head 

//   val Seq(tname,vname,fname,aname) = Seq(
//     ("tableName",t,str),
//     ("typeName",v,str),
//     ("fkeyName",f,str),
//     ("attrName",a,str)
//   ).map(simpleacsets.Attr.apply)
//   val attrs = Seq(tname,vname,fname,aname)
//   val elts = tables ++ fkeys

//   val S = SimpleSchema(
//     tables ++ fkeys ++ attrs:_*
//   )

// val schSchema = Schemas.S


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
    
//     case FKeySrc extends SchemaHom(FKey,Table)
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


// val schemaDisplay = GraphDisplay(
//   schSchema,  
//   Seq(
//     Schemas.t -> ShapeNode(Schemas.tname,PropMap(Schemas.tname -> "Hi")),
//     Schemas.v -> (ShapeNode(Schemas.vname),PropMap(ShapeProp.Shape -> ShapeOption.DiscShape,Fill -> "blue")),
//     Schemas.f -> (Schemas.fsrc,Schemas.ftgt,Arrow(Schemas.fname)),
//     Schemas.a -> (Schemas.asrc,Schemas.atgt,Arrow(Schemas.aname),PropMap() + (Stroke,"purple")),
//   )
// )

