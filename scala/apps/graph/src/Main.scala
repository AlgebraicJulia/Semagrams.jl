package semagrams.graph

import semagrams._
import semagrams.util._
import semagrams.acsets.abstr._
import semagrams.acsets.simple._
import semagrams.bindings._
import semagrams.graphs._
import semagrams.sprites._
import semagrams.acsets.simple.SimpleACSet.simpleACSetIsACSet
// import semagrams.api._
// import semagrams.graphs._
import com.raquo.laminar.api.L._


import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js


implicit val schIsSchema: Schema[SchGraph.type] = 
  schGraphIsSchema
implicit val propsAreData: PartData[PropMap] = 
  PartData.propsAreData
implicit val acsetIsACSet: ACSetWithSchemaAndData[SchGraph.type,PropMap][SimpleACSet[SchGraph.type]] =
  simpleACSetIsACSet[SchGraph.type]

val bindings = Seq[Binding[SimpleACSet[SchGraph.type]]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
  Binding(
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift), 
    AddEdgeViaDrag(E, Src, Tgt)
  ),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),Callback(part => 
      pTable.edit(part,Content)
  )),
  Binding(KeyDownHook("?"), PrintModel()),
)



val graphdisplay = GraphDisplay[PropMap](
  Seq(VertexDef(Rect(Content),PropMap() + (Fill,RGB("green")) + (Content,"Hi"),V)),
  Seq(EdgeDef(Arrow(Content),PropMap().set(Stroke,RGB("purple")), E -> (Src,Tgt)))
)

val sema: SemagramElt[PropMap,SimpleACSet[SchGraph.type]] = 
  graphdisplay(bindings,SimpleACSet(SchGraph))



val semaAttrs = Seq(
  backgroundColor := "white",
  height := "400px",
  width := "100%",
  border := "black",
  borderStyle := "solid",
  backgroundColor := "white",
  boxSizing := "border-box",
)



val pTable = sema.propTable(V,Content,Fill)



object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaAttrs,
        ),
        pTable.elt,
      )
        
      render(mountInto, mainDiv)
    }
  }
}
