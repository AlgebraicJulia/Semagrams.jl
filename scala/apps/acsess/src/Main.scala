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
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift).filter(TableOb), 
    AddEdgeViaDrag[AS](
      TableOb -> (FKeyOb,FKeySrc,FKeyTgt),
      // TableOb -> (ColumnOb,ColumnSrc,ColumnTgt),
      ValTypeOb -> (ColumnOb,ColumnSrc,ColumnTgt)
    )
  ),

  Binding(
    DoubleClickOnPartHook(MouseButton.Left).filter(TableOb),
    Callback(schTable.edit(_,Content))
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

  
def schStateMsg(es:EditorState,schACSet:AS): Message[AS] =
  val hovMsg = es.hovered.collect{ 
    case part:Part if part != backgroundPart => SetPropsMsg[AS](part,schHoverProps(part.ob))
  }
  val selectMsgs = es.selected.map(part =>
    SetPropsMsg[AS](part,schSelectProps(part.ob))  
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
      ValTypeOb -> PropMap().set(Fill,RGB("white")).set(Stroke,RGB("white"))
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
  SimpleSchema().toACSet(SchemaDisplay()),
)

val schTable = schSema.propTable(TableOb,Content,Fill,Selected,Highlight,Hovered)

def propSig: Signal[Map[UUID,PropMap]] = schSema.signal.map( (acset,state) =>
  acset.getProps(state.selected).map((part,props) => (part.id,props))
)


def propsObs(cols:Property*): Observer[(UUID,PropChange[_])] = Observer((id0,change) => 
  if !cols.contains(change.prop)
  then ()
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
    // .map((state,acset) => (state,acset.schema))

lazy val stateObs: Observer[Message[EditorState]] = acsetSema.messageBus.writer
    .contramap(edMsg =>
      Left(stateUpdate(edMsg))
    )


val selectIO: IO[Option[(Ob,PropMap)]] = IO {
  schSema.getState().selected.map(_.id) match
    case Seq(id0) => 
      val props = schSema.getModel().getProps(Part(id0,TableOb))
      val table = Table(id0,props.get(Content).getOrElse("")) 
      Some(table -> props)          
    case _ => None
}

val acsetBindings: Seq[Binding[AA]] = 
  Seq(
  Binding(
    KeyDownHook("a"),
    AddAtMouse(selectIO,select = false)
  ),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left),MoveViaDrag()),
  Binding(KeyDownHook("?"), PrintModel()),
)




val acsetDisplay = GraphDisplay[PropMap,AA](
  Seq(VertexDef(
    Rect(Content,PropMap() + (MinimumWidth,20) + (MinimumHeight,20)),
    {case t:Table => PropMap().set(Fill,RGB("pink"))}
  )),
  Seq()
)

val acsetSema = acsetDisplay(
  acsetBindings,
  SimpleACSet()
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


def acsetTableSig(cols:Property*) = schSema.stateSig
  .map(_.selected.flatMap(part => acsetSema.getModel().schema.tryOb(part.id)))
  .split(x => x)((table,_,_) =>
    acsetSema.propTable(table,cols:_*).elt
  )

def acsetTables = acsetTableSig(Content,Fill,Selected,Highlight,Hovered)
  // .
  // .combineWith(acsetSema.modelSig)
  //   .map( (schState,acset) => 
  //     println(s"acsetTableSig")
  //     for
  //     id0 <- schState.selected.map(_.id)
  //     table <- acset.schema.obs.find(_.id == id0)
  //   yield acsetSema.propTable(table,cols:_*))
  

val changeSig = schSema.modelEvents.collect{ 
  case ChangePropMsg(part,change) => part.id -> change
}
val changeObs = propsObs(Content,Fill,Highlight,Selected)


object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {



      val mainDiv: Div = div(
        idAttr := "mainDiv",
        div(
          div(
            cls := "schemaDiv",
            display := "flex",
            schSema.elt.amend(
              flex := "1",
              width := "70%",
            ),
            schTable.elt.amend(
              flex := "1",
            ),
          ),
          child <-- schSema.stateEvents
            .collect{ case hov:HoverMsg => hov.toString},
          child <-- schSema.stateEvents
            .collect{ case hov:SelectMsg => hov.toString}
        ),
        div(
          "ACSet",
          acsetSema.elt,
          changeSig --> changeObs,
          stateStream --> stateObs
          // stateModifier
          // child <-- schSema.
        ),
        children <-- acsetTables
        // .map(tables =>
        //   tables.map()
        //   _.map(_.elt)),
      )
        
      render(mountInto, mainDiv)
    }
  }
}


