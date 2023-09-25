package semagrams.graph

import semagrams._
import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._

import Graphs._

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js


val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
  Binding(
    ClickOnPartHook(MouseButton.Left, KeyModifier.Shift), 
    AddEdgeViaDrag(E, Src, Tgt)
  ),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),PartCallback(
    part => pTable.edit(part,Content)
  )),
  Binding(KeyDownHook("?"), PrintModel)
)


val sema: SemagramElt = GraphDisplay(
  SchGraph,
  Seq(VertexDef(V,Rect(Content),PropMap() + (Fill,"red"))),
  Seq(EdgeDef(E,Src,Tgt,Arrow(Content),PropMap() + (Stroke,"purple")))
)(bindings)

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
  (m:Message[ACSet]) =>
    sema.update(a => m.execute(a))
)


val pTable = sema.propTable(V,Seq(Content,Center,Fill,Content))



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
        pTable.laminarElt(sema.signal,messenger),
      )
        
      render(mountInto, mainDiv)
    }
  }
}
