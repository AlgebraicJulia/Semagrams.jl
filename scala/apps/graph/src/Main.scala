package semagrams.graph

import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js

import com.raquo.laminar.api.L._


import semagrams._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.graphs._










/* Display */

/* A sprite turns a `PropMap` into svg code */
val nodeSprite = Rect(Content,PropMap() + (Fill -> "lightgreen"))
val edgeSprite = Arrow(Content,PropMap())


/* Some custom attributes */
// val srcName = Attr(UUID("CustomAttr"),"srcName",E,ValType[String]("String"))
// val tgtName = Attr(UUID("CustomAttr"),"tgtName",E,ValType[String]("String"))


/* Optional modification the acset based on current `EditorState` */
def layout[D:PartData](state:EditorState,acset:ACSet[D]) = 
  GraphDisplay.defaultLayout(state,acset)
    .softSetProp(Content,acset.getParts(V).map(p => p -> p.id.toString))
  



/* State management */

/* Initialize the model variable */
val graphVar = UndoableVar(Graph())


/* A `Binding` ties an `EventHook` to an `Action` */ 
val bindings = Seq[Binding[ACSet[PropMap]]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left).filter(V), MoveViaDrag()),
  Binding(
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift), 
    AddEdgeViaDrag((V,V),(E,Src,Tgt))
  ),
  Binding(DoubleClickOnPartHook(),Callback(part => 
    println(part)
    graphSema.tableVar.now().get(part.ob.id) match
      case Some(sematable) => 
        println(s"Some table $part")
        sematable.edit(part,Content)
      case None => 
        println(s"None $part")
        ()
  )),
  Binding(KeyDownHook("?"), PrintModel()),
  Binding(
    KeyDownHook("z",KeyModifier.Ctrl),
    Callback((_:Unit) =>
      graphVar.undo()
    )
  )
)






/* Construct the semagram */
val graphSema: ACSemagram[PropMap] = GraphDisplay[PropMap](
  /* Model variable */
  graphVar,
  /* Bindings for interaction */                              
  bindings,
  /* Vertex construction */
  VertexDef(V,nodeSprite),
  /* Edge construction */
  EdgeDef(E,Src,Tgt,edgeSprite),
  /* Optional pre-rendering */ 
  // layout
)





def initialize() = 
  /* Add styling to the main svg window */
  graphSema.elt.amend(
    backgroundColor := "lightblue"
  )


  /* Construct table elements and add styling */
  val graphTables = Map(
    V.id -> graphSema.editTable(V,Center,Content,Fill),
    E.id -> graphSema.editTable(E,SrcName,TgtName,Content,Stroke,Fill,StrokeWidth)
  )
  
  for (part,table) <- graphTables
  yield table.elt.amend(
    backgroundColor := "blue"
  )

  /* Attach tables to semagram's `tableVar` variable */ 
  graphSema.tableVar.set(graphTables)










/* The main app that is exported to javascript */
object Main {

  /* The graph editor application */
  @JSExportTopLevel("GraphApp")
  object GraphApp {    

    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {

      
      initialize()
      
      val mainDiv: Div = div( 
        idAttr := "graph-app",
        /* The svg window */
        graphSema.elt,
        /* A family of tables */
        div(
          idAttr := "graph-app-tables",
          display := "flex",
          children <-- graphSema.tableVar.signal.map(tableMap =>
            tableMap.toSeq.map((_,table) => table.elt)
          )
        )
      )
      
      /* Render the laminar element to the DOM */
      render(mountInto, mainDiv)
    }
  }
}





