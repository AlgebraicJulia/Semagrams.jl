package semagrams.acsess

import semagrams._
import semagrams.acsets._
import semagrams.graphs._
import semagrams.state._
import semagrams.bindings._
import semagrams.util._
import semagrams.rendering._

import com.raquo.laminar.api.L._


import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js
import cats.effect.IO



def makeName() = scala.util.Random.alphanumeric.dropWhile(_.isDigit).head.toString
def makeColor(): RGB = 
  val Seq(r,g,b) = (1 to 3).map(_ => scala.util.Random.between(20,256))
  if r + g + b < 25 then makeColor() else RGB(r,g,b)

  


/* Schema editor */


/* Display */

val rectSprite = Rect(Content,PropMap())
val edgeSprite = Arrow(Content,PropMap() + (Stroke -> "purple"))



def schemaLayout[D:PartData](acset:ACSet[D],state:EditorState) = 
  acset.setProp(Highlight,state.hoveredPart.map(_ -> ()))
    .setProp(SrcName, 
      acset.mapProp(FKeySrc,
        part => acset.tryProp(Content,part).getOrElse(part.toString)
      ) ++
      acset.mapProp(AttrSrc,
        part => acset.tryProp(Content,part).getOrElse(part.toString)
      )
    )
    .setProp(TgtName, 
      acset.mapProp(FKeyTgt,
        part => acset.tryProp(Content,part).getOrElse(part.toString)
      ) ++
      acset.mapProp(AttrTgt,
        part => acset.tryProp(Content,part).getOrElse(part.toString)
      )
    )
    .softSetProp(Content, 
      acset.getParts(ValTypeOb).map(part => part -> part.id.toString)
    )  

/* State management */
type A = ACSet[PropMap]

val schemaVar = UndoableVar(ACSet(SchSchema))

val tablePropsIO = IO(
  PropMap() 
    + (Content -> makeName()) 
    + (Fill -> makeColor())
)

val schemaBindings = Seq[Binding[ACSet[PropMap]]](
  Binding(
    KeyDownHook("a"),
    AddAtMouse(TableOb,tablePropsIO)
  ),
  Binding(KeyDownHook("A",KeyModifier.Shift),AddAtMouse(ValTypeOb)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(
    ClickOnPartHook(MouseButton.Left).filter(TableOb,ValTypeOb), 
    MoveViaDrag().andThen(_ => IO(acsetVar.save()))
  ),
  Binding(
    ClickOnPartHook(MouseButton.Left,KeyModifier.Shift).filter(TableOb),
    AddSpanViaDrag(
      (TableOb,TableOb) -> (FKeyOb,FKeySrc,FKeyTgt,PartData()),
      (TableOb,ValTypeOb) -> (AttrOb,AttrSrc,AttrTgt,PartData()),
    )
  ),
  Binding(
    DoubleClickOnPartHook(MouseButton.Left),
    Callback((part:Part) =>
      schemaSema.tableVar.now()(part.ob.id).edit(part,Content)
    ),
  ),
  Binding(KeyDownHook("?"),PrintModel()),
  // How to keep these in sync?
  Binding(
    KeyDownHook("z",KeyModifier.Ctrl),
    Callback(() => {
      schemaVar.undo()
    })
  ),
  Binding(
    KeyDownHook("Z",KeyModifier.Ctrl,KeyModifier.Shift),
    Callback(() => 
      schemaVar.redo()
    )
  ),
)



val schVSrc = ObSource[PropMap](UID("SchVSrc"),Rect(),TableOb,ValTypeOb)

val schESrc = SpanSource[PropMap](schVSrc.id,Arrow(),
  Span(FKeySrc,FKeyTgt) -> PropMap().set(Stroke,"red"),
  Span(AttrSrc,AttrTgt) -> PropMap().set(Stroke,"blue")
)

val schemaSema: GraphDisplay[PropMap] = GraphDisplay[PropMap](
  schemaVar,
  schemaBindings,
  schVSrc,
  schESrc,
  schemaLayout,
)

/* ================================*/



val schemaSignal = schemaSema.modelVar.signal
  .splitOne(acsetSchema(acsetSchemaId))((sch,_,_) => sch).distinct

val schemaObs: Observer[Schema] = Observer(newSch =>
  val oldSch = acsetSema.modelVar.now().schema
  val rems = oldSch diff newSch
  val adds = newSch diff oldSch
  acsetVar.update(
    _ -- rems.values ++ adds.values
  )  
)





/* Instance Editor */

/* Display */


def acsetLayout[D:PartData](acset:ACSet[D],es:EditorState) = 
  GraphDisplay.defaultLayout[D](acset:ACSet[D],es:EditorState)


def acsetPostProc[D:PartData](ents:EntitySeq[D]) = 
  
  val schemaProps = Seq(TableOb,ValTypeOb,FKeyOb,AttrOb).map(elt =>
    elt -> schemaVar.now().getProps(elt).mapKeys(_.id).toMap
  ).toMap
  
  ents.map(_ match
    case (tag:ObTag) -> (spr,data) =>
      tag -> (spr,data.softSetProps(schemaProps(TableOb)(tag.part.ob.id)))
    case (tag:HomTag) -> (spr,data) => 
      tag -> (spr,data.softSetProps(schemaProps(FKeyOb)(tag.hom.id)))
    case x => x
  )

/* State management */


val acsetSchemaId = UID("ACSetSch")
val acsetVar = UndoableVar(ACSet(Schema(acsetSchemaId)))



val selectIO: IO[Option[(Ob,PropMap)]] = IO {
  
  val (state,acset) = schemaSema.readout()
  val schema = acsetSema.modelVar.now().schema
  
  for
    part <- state.selected.filter(_.ob == TableOb).headOption
    props = acset.getProps(part)
    ob <- schema.tryTable(part.id)
  yield ob -> PropMap()//props
  
}

val schemaIsHovered = () => acsetSema.isHovered


val acsetBindings = Seq[Binding[ACSet[PropMap]]](
  Binding(
    KeyDownHook("a").filter(schemaIsHovered),
    AddAtMouse(selectIO)
  ),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(
    ClickOnPartHook(MouseButton.Left).filter(_.ob.isInstanceOf[Table]),
    MoveViaDrag()
  ),
  Binding(
    ClickOnPartHook(MouseButton.Left,KeyModifier.Shift).filter(_.ob.isInstanceOf[Table]),
    AddHomViaDrag(
      (s,t) => IO(
        acsetVar.now().schema.fkeySeq
          .filter(f => 
            println(s"filter 1 $f")
            println(schemaSema.stateVar.now().selected)
            println(schemaSema.stateVar.now().selected.contains(f))
            for g <- schemaSema.stateVar.now().selected
            yield
              println(s"comparing $f and $g")
              println(s"eq: ${f == g}")
              println(s"id: ${f.id == g.id}")
            schemaSema.stateVar.now().selected.exists(_.id == f.id))
          .filter(f => 
            println(s"filter 2 ${f.asInstanceOf[FKey].display}")
            println(s"s = $s, t = $t")
            f.dom == s & f.codom == t
          )
          .headOption
      )
    )
  ),
  Binding(
    KeyDownHook("z",KeyModifier.Ctrl),
    Callback(() => {
      acsetVar.undo()
    })
  ),
  Binding(
    KeyDownHook("Z",KeyModifier.Ctrl,KeyModifier.Shift),
    Callback(() => 
      acsetVar.redo()
    )
  ),
  Binding(KeyDownHook("?"),PrintModel())
)




def initialize() =
  val schemaTables = Map(
    TableOb.id -> schemaSema.editTable(TableOb,Content,Fill),
    FKeyOb.id -> schemaSema.editTable(FKeyOb,SrcName,TgtName,Content,Stroke)
  )

  schemaSema.tableVar.set(schemaTables)




def vProps = PropMap() 
  + (MinimumHeight,25) + (MinimumWidth,25)
  + (InnerSep,0)

def eProps = PropMap()
  + (Stroke,"black")

val acsetVSrc = ObSource(
  UID("ACSetVSrc"),
  rectSprite,
  { case t:Table => vProps }
)

val acsetESrc = HomSource(
    UID("ACSetESrc") -> (acsetVSrc.id,acsetVSrc.id),
    Arrow(),
    { _ => PartData() }
)



val acsetSema: GraphDisplay[PropMap] = GraphDisplay[PropMap](
  acsetVar,
  acsetBindings,
  acsetVSrc,
  acsetESrc,
  acsetLayout,
  acsetPostProc
)



object Main {
  @JSExportTopLevel("AcsessApp")
  object AcsessApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {

      initialize()

      val mainDiv: Div = div(
        idAttr := "acsess-app",
        div(
          cls := "acsess-schema",
          display := "flex",
          schemaSema.elt.amend(
            flex := "1",
          ),
          div(
            children <-- schemaSema.tableVar.signal.map(tableMap =>
              tableMap.toSeq.map((_,table) => table.elt.amend(
                maxHeight := "400px",
                overflow := "auto",  
              ))
            ),
            child <-- schemaSema.stateVar.signal.map(_.selected.toString()),
          ),
          padding := "10px",
        ),
        div(
          cls := "acsess-instance",
          display := "flex",
          acsetSema.elt.amend(
            flex := "1",
          ),
          children <-- acsetSema.tableVar.signal.map(tableMap =>
            tableMap.toSeq.map((_,table) => table.elt.amend(
              maxHeight := "400px",
              overflow := "auto",  
            ))
          ),
          schemaSignal.changes --> schemaObs,
          padding := "10px",
        ),
        div(
          flex := "column",

        )
      )
        
      render(mountInto, mainDiv)
    }
  }
}
