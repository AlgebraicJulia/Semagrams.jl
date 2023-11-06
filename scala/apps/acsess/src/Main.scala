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
import semagrams.MouseButton
import semagrams.widgets.EditMsg



type AS = SimpleACSet[SchSchema.type]
type AA = SimpleACSet[SimpleSchema]




implicit val schSchemaDef: Schema[SchSchema.type] = schSchemaIsSchema
implicit val schACSetDef: ACSetWithSchema[SchSchema.type][AS] & ACSetWithData[PropMap][AS]
//  & ACSetWithData[PropMap][AS] 
= simpleACSetIsACSet(SchSchema)

implicit val simpleSchemaDef: Schema[SimpleSchema] = simpleSchemaIsSchema
implicit val acsetDef: ACSetWithSchema[SimpleSchema][AA] & ACSetWithData[PropMap][AA] = simpleACSetIsACSet[SimpleSchema]





object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {



      val mainDiv: Div = div(
        idAttr := "mainDiv",          
        div(
          cls := "schemaDiv",
          display := "flex",
          schSema.elt.amend(
            flex := "1",
            // width := "70%",
          ),
          padding := "10px",
          schemaTablesElt.amend(
            maxHeight := "400px",
            overflow := "auto",
          ),
        ),
        div(
          acsetSema.elt,
          tableStream --> tableObs,
          changeSig --> changeObs,
          stateStream --> stateObs,
          padding := "10px",
        ),
        children <-- acsetTables,
        child <-- acsetSema.modelSig.map(_.schema.toString)
      )
        
      render(mountInto, mainDiv)
    }
  }
}


val tableVar: Var[Map[UUID,Table]] = Var(Map())

val schACSetVar = Var(SimpleACSet(SchSchema))

val tableStream = schACSetVar.signal.map(acset =>
  acset.getProp(Content,TableOb).map( (part,name) =>
    part.id -> Table(part.id,name)
  )
)

val acsetVar = Var(SimpleACSet(SimpleSchema()))

val tableObs = Observer((newTables:Map[UUID,Table]) =>
  val oldTables = tableVar.now()
  val adds = newTables -- oldTables.keys
  val rems = oldTables -- newTables.keys
  val changes = (oldTables.keySet intersect newTables.keySet)
    .filter(k => oldTables(k) != newTables(k))
    .map(id => id -> newTables(id))
  
  acsetVar.update(acset => 
    acset.addSchemaElts(adds.values.toSeq)  
      .remSchemaElts(rems.keys.toSeq)
      .replaceSchemaElts(changes.toSeq)
  
  )
)





def makeName() = scala.util.Random.alphanumeric.dropWhile(_.isDigit).head.toString
def makeProps() = 
  val color = if math.random() > .5 then RGB("red") else RGB("blue")
  PropMap(Content -> makeName(),Fill -> color)



val schemaBindings: Seq[Binding[AS]] = Seq(
  Binding(
    KeyDownHook("a"),
    AddAtMouse(TableOb,IO(makeProps()))
  ),
  Binding(
    KeyDownHook("t"),
    AddAtMouse(ValTypeOb,IO{PropMap(Content -> makeName())})
  ),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(KeyDownHook("?"),PrintModel()),
  Binding(
    ClickOnPartHook(MouseButton.Left).filter(TableOb,ValTypeOb),
    MoveViaDrag()
  ),
  Binding[Part,AS](
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift)
      .filter(TableOb), 
    AddEdgeViaDrag[AS](
      _ match
        case TableOb => IO(Some(FKeyOb -> FKeySrc))
        case _ => IO(None)
      ,
      (_,_) match
        case (TableOb,TableOb) => IO(Some(
          (FKeyOb,FKeySrc,FKeyTgt,PropMap())
        ))
        case (TableOb,ValTypeOb) => IO(Some(
          (ColumnOb,ColumnSrc,ColumnTgt,PropMap())
        ))
        case _ => IO(None)
    )
  ),
  Binding(
    DoubleClickOnPartHook(MouseButton.Left),
    Callback(part => 
      schemaTables(part.ob).editObs.onNext(part -> Content)
    )
  )
)


val schHoverProps = Map[Ob,PropMap](
  TableOb -> PropMap(Hovered -> (), Highlight -> ()),
  ValTypeOb -> PropMap(Hovered -> (),Highlight -> ())
).withDefaultValue(PropMap())

val schSelectProps = Map[Ob,PropMap](
  TableOb -> PropMap(StrokeWidth -> 3,Selected -> ()),
  FKeyOb -> PropMap(StrokeWidth -> 3, Selected -> ())
).withDefaultValue(PropMap())

  
def schStateMsg[A:ACSet](es:EditorState,schACSet:A): Message[A] =
  val hovMsg = es.hovered.collect{ 
    case part:Part if part != backgroundPart => SetPropsMsg[A](part,schHoverProps(part.ob))
  }
  val selectMsgs = es.selected.map(part =>
    SetPropsMsg[A](part,schSelectProps(part.ob))  
  )
  MsgSeq(hovMsg.toSeq ++ selectMsgs)


val schemaDisplay = GraphDisplay[PropMap,AS](
  Seq(
    VertexDef[PropMap](
      Rect(Content),
      TableOb -> PropMap()
    ),
    VertexDef[PropMap](
      Disc(Content),
      ValTypeOb -> PropMap().set(Fill,RGB("white"))
    )
  ),
  Seq(
    EdgeDef(
      Arrow(Content),
      PropMap() + (Stroke,RGB("purple")),
      FKeyOb -> (FKeySrc,FKeyTgt)
    ),
    EdgeDef(
      Arrow(Content),
      PropMap() + (Stroke,RGB("cyan")),
      ColumnOb -> (ColumnSrc,ColumnTgt)
    ),
  ),
  schStateMsg
)



val schSema = schemaDisplay(
  schemaBindings,
  schACSetVar
)

val schemaTables: Map[Ob,schSema.SemaTable] = SchSchema.obs.map(ob =>
  ob -> schSema.propTable(ob,Content,Fill,Selected,Highlight,Hovered)
).toMap

val schemaTablesElt = div(
  cls := "schema-tables",
  flex := "1",
  children <-- schSema.stateSig.map(
    _.selected.map(_.ob).distinct
      .flatMap(schemaTables.get)
  ).split(table => table)( (table,_,_) =>
    table.elt
  )
)




def propSig: Signal[Map[UUID,PropMap]] = schSema.signal.map( (acset,state) =>
  acset.getProps(state.selected).map((part,props) => (part.id,props))
)


def propsObs(cols:Property*): Observer[(UUID,PropChange[_])] = Observer((id0,change) => 
  

  if !cols.contains(change.prop)
  then Message()
  else  
    val a = acsetSema.getModel()
    val propmsg = MsgSeq(for
      table <- a.schema.obs
      if id0 == table.id 
      (part,v0) <- a.tryProp(change.prop,table)
      if v0 == change.oldVal
    yield ChangePropMsg[AA](part,change))

    acsetSema.modelObs.onNext(propmsg)

)




// val stateMsgSig  = 
//   schSema.stateEvents.collect{ case msg: (HoverMsg | SelectMsg) => msg}



def tableParts[A:ACSet](tableId:UUID,acset:A): Seq[Part] = 
  acset.schema.obs.find(_.id == tableId).toSeq
    .flatMap(table => acset.getParts(table))


def getParts[A:ACSet](eOpt:Option[Entity],a:A): Seq[Part] =
  eOpt.collect{ case part:Part => part.id }
    .flatMap(tableId => a.schema.obs.find(_.id == tableId)).toSeq
    .flatMap(table => a.getParts(table))

def getTable[A:ACSet](eOpt:Option[Entity],a:A): Option[Ob] =
  eOpt.collect{ case part:Part => part.id }
    .flatMap(tableId => a.schema.obs.find(_.id == tableId))


def stateUpdate(msg:Message[EditorState]): Message[AA] = msg match
  case msg:HoverMsg =>  
    def remHighlight(a:AA) = getTable(msg.prev,a)
      .map(table => a.remProp(Highlight,table))
      .getOrElse(a)

    def addHighlight(a:AA) = getTable(msg.next,a)
      .map(table => a.setProp(Highlight,table,()))
      .getOrElse(a)
    
    FreeMsg(acset => addHighlight(remHighlight(acset)))

  case _ => 
    Message()


lazy val stateStream = schSema.stateEvents

lazy val stateObs: Observer[Message[EditorState]] = acsetSema.messageBus.writer
    .contramap(edMsg =>
      Left(stateUpdate(edMsg))
    )


val selectIO: IO[Option[(Ob,PropMap)]] = IO {
  schSema.getState().selected.map(_.id) match
    case Seq(id0) => 
      println(acsetSema.getModel().schema.obs)
      val props = schSema.getModel().getProps(Part(id0,TableOb))
      val table = Table(id0,props.get(Content).getOrElse("")) 
      Some(table -> props)          
    case _ => None
}

val acsetBindings: Seq[Binding[AA]] = 
  Seq(
  Binding(KeyDownHook("a"),AddAtMouse(selectIO)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(KeyDownHook("?"), PrintModel()),
  Binding(ClickOnPartHook(MouseButton.Left),MoveViaDrag()),
  Binding(
    DoubleClickOnPartHook(MouseButton.Left),
    Callback(part =>
      // acsetTables.
      val msg = EditMsg(None,Some((part,Content)))
      println(msg)  
    )
  ),
  Binding[Part,AA](
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift), 
    AddEdgeViaDrag[AA](
      dummyIO,
      (_,_) match
        case (TableOb,TableOb) => IO(Some(
          (FKeyOb,FKeySrc,FKeyTgt,PropMap())
        ))
        case (TableOb,ValTypeOb) => IO(Some(
          (ColumnOb,ColumnSrc,ColumnTgt,PropMap())
        ))
        case _ => IO(None)
    )
  ),
)

def dummyIO(ob:Ob): IO[Option[(Ob,GenHom[_])]] = IO {
  println(s"dummyIO $ob")
  val sch = acsetSema.getModel().schema

  sch.homs.filter(f => f.dom == ob)
    .headOption.map(f => f.dom -> f)
  
}


val acsetDisplay = GraphDisplay[PropMap,AA](
  Seq(VertexDef(
    Rect(Content,PropMap() + (MinimumWidth,20) + (MinimumHeight,20)),
    {case t:Table => PropMap().set(Fill,RGB("pink"))}
  )),
  Seq(),
  schStateMsg
)

val acsetSema = acsetDisplay(
  acsetBindings,
  // acsetVar
  Var(SimpleACSet())
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


def tableFromId(cols:Property*)(id0:UUID): Signal[Option[Element]] = 
  acsetSema.modelSig.map(acset =>
    acset.schema.tryOb(id0).map(table => 
      acsetSema.propTable(table,cols:_*).elt
    )     
  )

def tablesFromIds(cols:Property*)(ids:Seq[UUID]): Signal[Seq[Element]] =
  acsetSema.modelSig.map(acset => ids.flatMap(id0 =>
    acset.schema.tryOb(id0).map(table =>
      acsetSema.propTable(table,cols:_*).elt
    ) 
  ))


val at1 = acsetSema.modelSig.map(acset =>
  acset.schema.obMap.toSeq  
).split(_._1){ case (id,(_,ob),pairSig) =>
  acsetSema.propTable(ob)  
}





def acsetTableSig(cols:Property*) = schSema.stateSig
  .map(_.selected.flatMap(part => acsetSema.getModel().schema.tryOb(part.id)))
  .split(x => x)((table,_,_) =>
    acsetSema.propTable(table,cols:_*).elt
  )

def acsetTables = acsetTableSig(Content,Fill,Stroke,Selected,Highlight,Hovered)
  

val changeSig = schSema.modelEvents.collect{ 
  case ChangePropMsg(part,change) => part.id -> change
}
val changeObs = propsObs(Content,Fill,Highlight,Selected).contramap(
  (pair:(UUID,PropChange[_])) =>
    println(s"changeObs $pair")
    pair
)
