package semagrams.acsess

import semagrams._
import semagrams.api._
import semagrams.acsets.abstr._
import semagrams.graphs._
import semagrams.acsets.simple._
import semagrams.bindings._
import semagrams.util._

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import semagrams.acsets.simple.SimpleACSet.simpleACSetIsACSet
import cats.effect.IO



type AS = SimpleACSet[SchSchema.type]
type AA = SimpleACSet[SimpleSchema]




implicit val schSchemaDef: Schema[SchSchema.type] = schSchemaIsSchema
implicit val schACSetDef: ACSetWithSchema[SchSchema.type][AS] & ACSetWithData[PropMap][AS]
//  & ACSetWithData[PropMap][AS] 
= simpleACSetIsACSet(SchSchema)

implicit val simpleSchemaDef: Schema[SimpleSchema] = simpleSchemaIsSchema
implicit val acsetDef: ACSetWithSchema[SimpleSchema][AA] & ACSetWithData[PropMap][AA] = simpleACSetIsACSet[SimpleSchema]






val schemaBindings: Seq[Binding[AS]] = Seq(
  Binding(
    KeyDownHook("a"),
    AddAtMouse[AS](IO {
      val part = Part(UUID("SchemaTable"),TableOb)
      val props = PropMap()
        .set(Fill,if math.random() > .5 then RGB("red") else RGB("blue"))
        .set(Content,scala.util.Random.alphanumeric.dropWhile(_.isDigit).head.toString)
      part -> props
    })
  ),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(KeyDownHook("?"),PrintModel()),
  Binding(
    ClickOnPartHook(MouseButton.Left).filter(TableOb),
    MoveViaDrag()
  ),
  Binding[Part,AS](
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift).filter(TableOb), 
    AddEdgeViaDrag[AS](FKeyOb,FKeySrc,FKeyTgt)
  ),

  Binding(
    DoubleClickOnPartHook(MouseButton.Left).filter(TableOb),
    Callback(part =>
      println(s"callback $part")
      schTable.edit(part,Content)
    )
  ),
)




val schemaDisplay = GraphDisplay(
  Seq(
    VertexDef(
      Rect(Content),
      TableOb -> PropMap().set(Fill,RGB("green"))
    )
  ),
  // Seq()
  Seq(
    EdgeDef(
      Arrow(Content),
      PropMap() + (Stroke,RGB("purple")),
      FKeyOb -> (FKeySrc,FKeyTgt)
    )
  )
)


val schSema = schemaDisplay(
  schemaBindings,
  SimpleSchema().toACSet(SchemaDisplay())
)

val schTable = schSema.propTable(TableOb,Content,Fill,Selected,Hovered)


// def tables[A:ACSetWithSchema[SchSchema.type]](acset:A,tableIds:Seq[UUID] = Seq()): Seq[Table] = 
//   val ids = if tableIds.nonEmpty then tableIds else acset.getParts(TableOb).map(_.id)

//   acset.tryProp(Content,TableOb).toSeq
//     .filter( (part,_) => ids.contains(part.id))
//     .map( (part,nameOpt) =>
//       Table(part.id,nameOpt.getOrElse(""))
//     )


// def table[A:ACSetWithSchema[SchSchema.type]](acset:A,id:UUID): Table =
//   tables(acset,Seq(id)).head

// def tables[A:ACSetWithSchema[SchSchema.type]](acset:A,state:EditorState): Seq[Table] =
//   tables(acset,state.selected.map(_.id)) 

// def tableProps[A:ACSetWithSchema[SchSchema.type]](acset:A,tableIds:Seq[UUID] = Seq()): Map[Table,PropMap] =
//   tables(acset,tableIds).map(table => table -> acset.getProps(table.asPart())).toMap

// def tableProps[A:ACSetWithSchema[SchSchema.type]](acset:A,state:EditorState): Map[Table,PropMap] =
//   tableProps(acset,state.selected.map(_.id)) 

def propSig: Signal[Map[UUID,PropMap]] = schSema.signal.map( (acset,state) =>
  acset.getProps(state.selected).map((part,props) => (part.id,props))
)


def propsObs(cols:Property*): Observer[(UUID,PropChange[_])] = Observer((id0,change) => 
  if !cols.contains(change.prop)
  then
    println("miss")
    ()
  else  
    val a = acsetSema.getModel()
    val msg = MsgSeq(for
      table <- a.schema.obs
      if id0 == table.id 
      (part,v0) <- a.tryProp(change.prop,table)
      if v0 == change.oldVal
    yield ChangePropMsg[AA](part,change))

    acsetSema.modelObs.onNext(msg)

)


val selectIO: IO[Option[(Part,PropMap)]] = IO {
  schSema.getState().selected.map(_.id) match
    case Seq(id0) => 
      val props = schSema.getModel().getProps(Part(id0,TableOb))
      val table = Table(id0,props.get(Content).getOrElse("")) 
      Some(Part(table) -> props)          
    case _ => None
}

val acsetBindings: Seq[Binding[AA]] = 
  Seq(
  Binding(
    KeyDownHook("a"),
    AddAtMouse(selectIO,select = false)
  ),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left),MoveViaDrag())
)

val acsetDisplay = GraphDisplay(
  Seq(VertexDef(
    Rect(Content,PropMap() + (MinimumWidth,20) + (MinimumHeight,20)),
    {case t:Table => PropMap().set(Fill,RGB("pink"))}
  )),
  Seq()
)

val acsetSema = acsetDisplay(
  acsetBindings,
  SimpleACSet(),
  // acset => 
  //   // println("transforming")
  //   acset.setProps(Selected,
  //     getSelection().flatMap(table =>
  //       println(s"table $table")
  //       acset.getParts(table).map(_ -> ())
        
  //     )
  //   )

)







val semaAttrs = Seq(
  backgroundColor := "white",
  height := "400px",
  width := "70%",
  border := "black",
  borderStyle := "solid",
  backgroundColor := "white",
  boxSizing := "border-box",
)

// val selectSig1 = schSema.stateSig.map(_.selected.map(_.id))

def acsetTableSig(cols:Property*) = 
  schSema.stateSig.combineWith(acsetSema.modelSig)
    .map( (schState,acset) => for
      id0 <- schState.selected.map(_.id)
      table <- acset.schema.obs.find(_.id == id0)
    yield acsetSema.propTable(table,cols:_*))
  

val changeSig = schSema.modelEvents.collect{ 
  case ChangePropMsg(part,change) => part.id -> change
}
val changeObs = propsObs(Content,Fill)


object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {



      val mainDiv: Div = div(
        idAttr := "mainDiv",
        div(
          div(cls := "schemaDiv",
            display := "flex",
            schSema.elt.amend(
              flex := "1",
              width := "70%",
            ),
            schTable.elt.amend(
              flex := "1",
            ),
          )
        ),
        div(
          "ACSet",
          acsetSema.elt,
          changeSig --> changeObs,
          // child <-- acsetSig.map(_.elt),
          // acsetSig.signal.flatMap(_.signal) --> acsetVar.writer
          // child <-- acsetSemaSig.map(_.elt.amend(
          //   semaAttrs,
          // )),
          // tickStream.mapTo(currentTable.now()) --> Observer(println)
        ),
        children <-- acsetTableSig(Content,Fill,Selected,Hovered)
          .map(_.map(_.elt)),

        // p(child <-- selectSig1.map(
        //   "selectVar: " + _.mkString(", ")
        // )),
        // p(child <-- schSema.signal.map("Schema: " + SimpleSchema.fromACSet(_)._1.toString)),
        // p(child <-- schSema.signal.map("SchemaDisplay: " + SimpleSchema.fromACSet(_)._2.toString)),
        // p(child <-- schSema.signal.map("SchemaACSet: " + _)),
        // asSignal.changes --> Observer(println).contramap(
        //   (sch:SimpleSchema) => sch.toACSet(schemaVar.now()).getParts(TableOb)
        // )

      )
        
      render(mountInto, mainDiv)
    }
  }
}




// val acsetVar = Var(SimpleACSet[SimpleSchema]())
// val t1 = 
//   val t = Table("T1")
//   println(s"t1 def ${t.id}")
//   nameVar.update(_ + (t.id -> t.name))
//   selectVar.set(Seq(t.id))
//   acsetVar.update(_.addSchemaElt(t))
//   schSema.messenger.onNext(AddPartMsg(
//     TableOb,
//     PropMap().set(Content,t.name).set(Center,Complex(100,100)),
//     t.id
//   ))
//   t




// val acsetSema = nameSig.map(names =>

//   val tables = names.toSeq.map(Table.apply.tupled)
//   val gd = GraphDisplay(
//     tables.map(VertexDef(_,Rect(Content),PropMap())),
//     Seq()
//   )

//   val init = SimpleACSet(SimpleSchema()).addSchemaElts(tables)

//   gd(acsetBindings,init)

// )



// val acsetVar = Var(SimpleACSet[SimpleSchema]())
// val t1 = 
//   val t = Table("T1")
//   println(s"t1 def ${t.id}")
//   nameVar.update(_ + (t.id -> t.name))
//   selectVar.set(Seq(t.id))
//   acsetVar.update(_.addSchemaElt(t))
//   schSema.messenger.onNext(AddPartMsg(
//     TableOb,
//     PropMap().set(Content,t.name).set(Center,Complex(100,100)),
//     t.id
//   ))
//   t





// val acsetDispSig = selectVar.signal
//   .combineWith(nameVar.signal)
//   .map( pair => 
//     val (select,names) = pair
//     println(s"acsetDispSig")
//     for (id,name) <- names 
//     yield println(s"$id -> $name") 
    
//     val tables = names.toSeq.map(Table.apply.tupled)
//     GraphDisplay(
//       Seq(VertexDef(
//         tables,
//         Rect(Content),
//         PropMap() + (Fill,"red") + (Content,"Hi")
//       )),
//       Seq()
//     )
//   )

// val acsetSemaSig = acsetVar.signal
//   .combineWith(selectVar.signal)
//   .combineWith(nameVar.signal)
//   .map( triple => 
//     val (acset,select,names) = triple
//     // println(s"acsetDispSig")
//     // for (id,name) <- names 
//     // yield println(s"$id -> $name") 
    
//     val vdefs = names.toSeq.map( (id,name) => VertexDef(
//       Table(id,name),
//       Rect(Content),
//       PropMap() + (Fill,"red") + (Content,"Hi")
//     ))

//     val gd = GraphDisplay(
//       vdefs,
//       Seq()
//     )
//     gd(acsetBindings,acset)
//   )

// val acsetVar = Var(acsetInit)

// val acsetSig = acsetVar.signal 
//   .combineWith(nameVar.signal)
//   .combineWith(selectVar.signal)
//   .map((acset,names,select) =>

//     println(
//       s"acsetSig: names = $names, select = $select"
//     )

//     val bs = if names.isEmpty
//     then Seq()
//     else 
//       val (id,name) = names.toSeq.head
//       println(s"acsetSig $id -> $name")
//       Seq(
//       Binding(
//         KeyDownHook("b"),
//         AddAtMouse(
//           Table(id,name)
//         //   IO{
//         //   println("binding IO")
//         //   selectVar.now() match
//         //     case Seq(id) =>
//         //       val ret = Some(Table(id,nameVar.now()(id)))
//         //       println(ret)
//         //       ret
//         //     case _ =>
//         //       None
//         // }
//         )(acsetDef)
//       ),
//     )
//     val tables = names.map((id,name) => Table(id,name)).toSeq
//     val vdefs = tables.map(t =>
//       VertexDef(t,Rect(Content),PropMap())
//     )
//     println(s"tables = ${vdefs.length}")
//     val gd = GraphDisplay(
//       vdefs,
//       Seq()
//     )

//     gd(bs,acset)

//   )

// val acsetSig = acsetDisplaySig.map(acsetDisp =>
//   acsetDisp(acsetBindings,acsetInit)  
// )

//   sch
// )
  
  
//   Binding(KeyDownHook("b"), schemaVar.now().selected.toSeq match
//     case Seq(id:UUID) => 
//       case Some(ob) => 
        
//         AddAtMouse(ob)(acsetDef)
//       case None => 
//         println("Missing object ")
        
//         Die()
//     case _ => 
//       println("No selected table")
//       Die()  
//   ),
//   // Binding(
//   //   KeyDownHook("?"),
//   //   PrintModel()  
//   // ),
// )





// val currentTable: Var[Option[Table]] = Var(None)

// case class AddTableElt[A:ACSet]() extends Action[Unit, A] {
//   def apply(_p: Unit, r: Action.Resources[A]) = 
//     r.stateVar.now().hovered match
//     case Some(ent) => 
//       val pos = r.stateVar.now().mousePos match
//         case Complex(0,0) => r.stateVar.now().dims/2.0
//         case z => z
//       currentTable.now() match
//         case Some(table) =>      
//           IO(r.modelVar.update(
//             _.addPart(table, PropMap() + (Center -> pos))._1
//           ))
//         case None => IO(())
    
//     case None => IO(())
  

//   def description = s"add acset new part of selected type at current mouse position"
// }

// case class SetCurrentTable[A:ACSet]() extends Action[Part,A] {
//   def apply(p:Part,r:Action.Resources[A]) = 
//     println(s"SetCurrentTable $p")
//     val nm = r.modelVar.now().tryProp(Content,p)
//     val ob = nm match
//       case Some(s) => Table(s)
//       case None => Table()
//     println(nm)
//     currentTable.set(nm.map(Table.apply))
//     IO(
//       currentTable.set(nm.map(Table.apply))
//     )
//     // case Part(tableOb,id) => IO(currentTable.set(Some(tableOb)))
//     // case _ => IO(())
//   def description = s"select"
// }


// val acsetSemaSig = acsetVar.signal.map( acset =>

//     val acsetDisplay = GraphDisplay(
//       acset.schema.obs.map(ob =>
        
//         val tableParts = schemaSema.readout().getParts(tableOb)

//         val props = schemaSema.readout().getProps(tableOb).get(ob.id)
//         println(s"props = $props")
//         VertexDef(ob,Rect(Content),PropMap())),
//       Seq()
//     )

//     acsetDisplay(acsetBindings,acset)

// ) 






// val schemaBus = EventBus[SchemaMsg[_]]()

// sealed trait SchemaMsg[X]
// case class AddTable() extends SchemaMsg[Unit]
// case class DragTable() extends SchemaMsg[Part]



  
// case class SendMsg[X](msg:SchemaMsg[X]) extends Action[X,AS]:
//   def apply(x:X,r:Action.Resources[AS]) = (x,msg) match
//     case (_,AddTable()) => r.mousePos match 
//       case None => IO(())
//       case Some(z) => 
//         IO {
//           edStateVar.update((acset,b) =>
//             val id = UUID("SchemaTable")
//             val newTable = Table(id,"")


//             (acset.addSchemaElt(newTable),b.setProps(newTable,PropMap().set(Center,z)))
//           )
//         }
//     case (part:Part,DragTable()) => 
//       val t = Table(part.id,r.modelVar.now().getProp(Content,part)) 
//       for {
//         _ <- IO(println(s"DragTable $t"))
//         ctr <- IO(r.modelVar.now().tryProp(Center,part))
//         offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().getProp(Center, part))
//         _ = println(s"offset = $offset")
//         _ <- takeUntil(r.eventQueue)(
//           evt => 
//             println(s"takeUntil Main $evt")
            
//             evt match {
//             case Event.MouseMove(pos) => 
//               edStateVar.updateRight(disp => disp.setProp(t,Center,pos - offset))
//               IO(None)
//               // r.modelVar.update(_.setProp(Center, p, pos - offset))
            
//             case Event.MouseUp(_, _) => 
//               println("MouseUp")
//               IO(Some(()))
//             case Event.MouseLeave(backgroundPart) => 
//               println("MouseLeave")
//               IO(Some(()))
//             case x                   => 
//               println(s"missed $x")
//               IO(None)
//           })
//       } yield ()
//     case x =>
//       IO(println(s"missed $x"))


    //   for {
    //     ctr <- IO(edStateVar.now()._2.props(table).
      
    //   )
    //   offset <- IO(r.stateVar.now().mousePos - r.modelVar.now().getProp(Center, p))
    //   _ <- takeUntil(r.eventQueue)(
    //     evt => evt match {
    //       case Event.MouseMove(pos) =>
    //         IO(r.modelVar.update(_.setProp(Center, p, pos - offset))) >> IO(None)
    //       case Event.MouseUp(_, _) => IO(Some(()))
    //       case Event.MouseLeave(backgroundPart) => IO(Some(()))
    //       case _                   => IO(None)
    //     })
    // } yield ()

  // def description = "Custom messenger for Schema updates"








// Msg[A:ACSet](id:UUID,z:Complex,name:String = "",props:PropMap = PropMap())

// def addTableMsg(): AddTableMsg[AA] =
//   val id = UUID("SchemaTable")
//   val newTable = Table(id,"")
//   val newPart = Part(TableOb,id)
//   AddTable(id,)


// case class AddTable[A:ACSet](name:String = "") extends SchemaAction[A] with Action[Unit,A]:
//   def apply(p:Unit,r:Action.Resources[A]) = r.mousePos.map { z =>
//     val id = UUID("SchemaTable")
//     val newTable = Table(id,name)
//     val newPart = Part(TableOb,id)
//     val newProps = PropMap() + (Center,z) + (Content,name)
  
//   }.getOrElse(IO(()))
  
// (id:UUID,z:Complex,props:PropMap = PropMap())
//   extends SchemaMsg[A]:
//   def updateDisplay(disp:SchemaDisplay) = disp.copy(
//     props = disp.props + (newTable -> (newPart -> props.set(Center,z))),
//     selected = Seq(newTable)
//   )
//   def updateACSet(acset:A) = 
//     acset.addSchemaElt(newTable)


// val schACSetSig: Signal[Element] = acsetVar.signal
//   .combineWith(schemaVar.signal)
//   .splitOne( (acset,disp) => 
//     println(s"splitOne1 ${acset.schema.obs} // ${disp.props.keys}")
//     acset.schema.toACSet(disp))
//   ( (schACSet,pair,pairSig) => 
//     println(s"splitOne2 ${schACSet.getParts(TableOb)}")
//     div(
//       schemaDisplay(schemaBindings,schACSet).elt,
//       div(
//         schACSet.toString
//       )
//     )
//   )


// val schemaSig = acsetVar.signal.map(_.schema)
//   .combineWith(schemaVar.signal)
//   .splitOne( (sch:SimpleSchema,schDisplay:SchemaDisplay) =>
//     sch.toACSet(schDisplay)
//   )( (acset,pair,pairSig) =>
//     schemaDisplay(schemaBindings,acset)  
//   )

// case class SendMsg[S:Schema]() extends Action[Unit,A]:

//   /* Activated from within the schema semagram */
//   def apply(_p:Unit,r:Action.Resources[A]) = r.mousePos match
//     case None => IO(())
//     case Some(z) =>
//       IO{
//         val newTable = Table("")
//         // println(s"newTable $newTable")
//         // println(acsetVar.now().schema.obs)
//         // println(schemaVar.now().props)
//         schemaVar.update(schDisp => schDisp.copy(
//           props = schDisp.props + (newTable -> (
//             Part(TableOb,newTable.id) -> PropMap().set(Center,z)
//           ))
//         ))
//         acsetVar.update(_.addSchemaElt(newTable))
//       }

//   def description = "Add schema table"

  


// val acsetBindings = Seq(
//   Binding(KeyDownHook("b"), schemaVar.now().selected.toSeq match
//     case Seq(t:Table) => AddAtMouse(t)(acsetDef)
//     case _ => 
//       println("No selected table")
//       Die()  
//   ),
//   // Binding(
//   //   KeyDownHook("?"),
//   //   PrintModel()  
//   // ),
// )






// val currentTable: Var[Option[Table]] = Var(None)

// case class AddTableElt[A:ACSet]() extends Action[Unit, A] {
//   def apply(_p: Unit, r: Action.Resources[A]) = 
//     r.stateVar.now().hovered match
//     case Some(ent) => 
//       val pos = r.stateVar.now().mousePos match
//         case Complex(0,0) => r.stateVar.now().dims/2.0
//         case z => z
//       currentTable.now() match
//         case Some(table) =>      
//           IO(r.modelVar.update(
//             _.addPart(table, PropMap() + (Center -> pos))._1
//           ))
//         case None => IO(())
    
//     case None => IO(())
  

//   def description = s"add acset new part of selected type at current mouse position"
// }

// case class SetCurrentTable[A:ACSet]() extends Action[Part,A] {
//   def apply(p:Part,r:Action.Resources[A]) = 
//     println(s"SetCurrentTable $p")
//     val nm = r.modelVar.now().tryProp(Content,p)
//     val ob = nm match
//       case Some(s) => Table(s)
//       case None => Table()
//     println(nm)
//     currentTable.set(nm.map(Table.apply))
//     IO(
//       currentTable.set(nm.map(Table.apply))
//     )
//     // case Part(tableOb,id) => IO(currentTable.set(Some(tableOb)))
//     // case _ => IO(())
//   def description = s"select"
// }


// val acsetSemaSig = acsetVar.signal.map( acset =>

//     val acsetDisplay = GraphDisplay(
//       acset.schema.obs.map(ob =>
        
//         val tableParts = schemaSema.readout().getParts(tableOb)

//         val props = schemaSema.readout().getProps(tableOb).get(ob.id)
//         println(s"props = $props")
//         VertexDef(ob,Rect(Content),PropMap())),
//       Seq()
//     )

//     acsetDisplay(acsetBindings,acset)

// ) 


// val messenger = Observer(
//   (m:Message[SimpleACSet[SimpleSchema]]) =>
//     acset.set(m.execute(acset.now()))
// )




// val schemaBindings = Seq[Binding[SimpleACSet[SimpleSchema]]](
//   Binding(KeyDownHook("acset"), AddAtMouse(tableOb)),
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
//       Schemas.v -> (Schemas.acset,Schemas.asrc,Schemas.atgt)
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
//         case Schemas.acset => attrTable.edit(p,Schemas.aname)
//       case _ => ()
//   )),
//   Binding(KeyDownHook("?"), PrintModel()),
// )


// val acsetdisplaySig = acset.signal.map(acset =>
//   GraphDisplay(
//     implicitly[ACSet[SimpleACSet[SimpleSchema]]](acset).sch,
//     s
//   )  
// )




// object Schemas:
//   // val Seq(T,V,F,A) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
//   val Seq(t,v,f,acset) = Seq("Table","ValType","FKey","Attr").map(SimpleTable.apply)
//   val tables = Seq(t,v,f,acset)

//   val Seq(fsrc,ftgt,asrc,atgt) = Seq(
//     ("fkeySrc",f,t),
//     ("fkeyTgt",f,t),
//     ("attrSrc",acset,t),
//     ("attrTgt",acset,v)
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
//     ("attrName",acset,str)
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
//         val fname = acset.props.get(Name).getOrElse(UUID("FKey"))
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


// val (Table,ValType,FKey,Attr) = (SchemaStuff.t,SchemaStuff.v,SchemaStuff.f,SchemaStuff.acset)


// val schemaDisplay = GraphDisplay(
//   schSchema,  
//   Seq(
//     Schemas.t -> ShapeNode(Schemas.tname,PropMap(Schemas.tname -> "Hi")),
//     Schemas.v -> (ShapeNode(Schemas.vname),PropMap(ShapeProp.Shape -> ShapeOption.DiscShape,Fill -> "blue")),
//     Schemas.f -> (Schemas.fsrc,Schemas.ftgt,Arrow(Schemas.fname)),
//     Schemas.acset -> (Schemas.asrc,Schemas.atgt,Arrow(Schemas.aname),PropMap() + (Stroke,"purple")),
//   )
// )

