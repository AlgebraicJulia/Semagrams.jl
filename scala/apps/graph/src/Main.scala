package semagrams.graph

import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js

import com.raquo.laminar.api.L._


import semagrams._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.graphs._



object Main {

  /* The graph editor application */
  @JSExportTopLevel("GraphApp")
  object GraphApp {    

    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {

      
      initialize()



      

      
      val mainDiv: Div = div( 
        idAttr := "graph-app",
        graphSema.laminarElt.amend(
          semaAttrs,
        ),
        div(
          idAttr := "graph-app-tables",
          display := "flex",
          children <-- graphSema.tableVar.signal.map(tableMap =>
            tableMap.toSeq.map((_,table) => table.elt)
          )
        ),
        // div(graphTables.values.toSeq.map(_.elt)),
        div(child <-- graphSema.modelSig.map(_.partStore(V.id).ids.mkString("->")))
      )
        
      render(mountInto, mainDiv)
    }
  }
}


/* Create table elements and attach them to the semagram tables variable */
def initialize() = 
  val graphTables = Map(
    V.id -> graphSema.editTable(V,Center,Content,Fill),
    E.id -> graphSema.editTable(E,srcName,tgtName,Content,Stroke,StrokeWidth)
  )

  for table <- graphTables.values
  yield table.elt.amend(
    backgroundColor := "blue"
  )
  
  graphSema.tableVar.set(graphTables)



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
)


val srcName = Attr(UUID("CustomAttr"),"srcName",E,ValType[String]("String"))
val tgtName = Attr(UUID("CustomAttr"),"tgtName",E,ValType[String]("String"))

val graphdisplay = GraphDisplay[PropMap](
  Seq(VertexDef(Rect(Content),PropMap() + (Fill,RGB("green")) + (Content,"Hi"),V)),
  Seq(EdgeDef(Arrow(Content),E,Src,Tgt)),
  // Seq(EdgeDef(Arrow(Content),PropMap().set(Stroke,RGB("purple")), E -> (Src,Tgt)))
  (state,acset) => acset
      .softSetProp(Content,acset.getParts(V).map(p => p -> p.id.toString))
      .setProp(srcName,acset.collectProp(Src,E).view.mapValues(_.toString))
      .setProp(tgtName,acset.collectProp(Tgt,E).view.mapValues(_.toString))
      .setProp(Highlight,state.hoveredPart.map(_ -> ()))
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


val g = UndoableVar(Graph())




val graphSema: ACSemagram[PropMap] = graphdisplay(bindings,g)




