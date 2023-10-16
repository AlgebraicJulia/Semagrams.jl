package semagrams.graph

import semagrams._
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
implicit val acsetIsACSet: ACSetWithSchAndData2[SchGraph.type,PropMap][SimpleACSet[SchGraph.type]] =
  simpleACSetIsACSet[SchGraph.type]
// implicit val acsetIsACSet: ACSetWithSch[
//   SchGraph.type,
//   // PropMap
//   ][
//   SimpleACSet[SchGraph.type]
// ] = 
//   SimpleACSet.simpleACSetIsACSet(SchGraph)

val bindings = Seq[Binding[SimpleACSet[SchGraph.type]]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
  Binding(
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift), 
    AddEdgeViaDrag(E, Src, Tgt)
  ),
  Binding(MsgHook(),ProcessMsg()),
  // Binding(DoubleClickOnPartHook(),PartCallback(
  //   part => pTable.edit(part,Content)
  // )),
  Binding(KeyDownHook("?"), PrintModel())
)



val graphdisplay = GraphDisplay[SchGraph.type,PropMap,SimpleACSet[SchGraph.type]](
  SchGraph,
  Seq(VertexDef(V,Rect(Content),PropMap() + (Fill,"red") + (Content,"Hi"))),
  Seq(EdgeDef(E,Src,Tgt,Arrow(Content),PropMap() + (Stroke,"purple")))
)

val sema1: SemagramElt[SchGraph.type,PropMap,SimpleACSet[SchGraph.type]] = 
  graphdisplay(bindings,SimpleACSet(SchGraph))

val sema2: SemagramElt[SchGraph.type,PropMap,SimpleACSet[SchGraph.type]] = 
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

val messenger = Observer(
  (m:Message[SimpleACSet[SchGraph.type]]) =>
    sema1.update(a => m.execute(a))
)


// val pTable = sema.propTable(V,Seq(Content,Center,Fill,Content))



object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema1.elt.amend(
          semaAttrs,
        ),
        sema2.elt.amend(
          semaAttrs,
        ),
        // pTable.laminarElt(sema.signal,messenger),
      )
        
      render(mountInto, mainDiv)
    }
  }
}
