package semagrams.graph

import semagrams._
import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.widgets._

import Graphs._

import com.raquo.laminar.api.L._


import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js

object GraphDisplay extends ACSemagram:
  type Model = ACSet

  def layout(g: ACSet) = assignBends(Map(E -> (Src, Tgt)), 0.5)(g)

  val entitySources = Seq(
    ACSetEntitySource(V, Disc()),
    ACSetEdgeSource(E, Src, Tgt, Arrow()),
  )

  val schema: Schema = SchGraph


val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left, Set(KeyModifier.Shift)), AddEdgeViaDrag(E, Src, Tgt)),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),PartCallback(
    part => propTable.edit(part,Content)
  )),
  Binding(KeyDownHook("?"), PrintModel)
)

val sema = GraphDisplay(bindings)

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
  (m:Message[ACSet]) => sema.update(a => m.execute(a))
)


val propTable = PropTable(V,Seq(Content,Center,Fill))



object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {

      // val cellSig = sema.signal
      //   .map(_.parts(ROOT,V).headOption)
      //   .splitOption({
      //     case ((part,acset),pairSig) => PropCell2(part,Center)
      //       .laminarElt(
      //         pairSig.map(pair => pair._2.props.get(Center)),
      //         messenger.contramap {
      //           case v:Complex => SetSubpartMsg(part,Center)(v)
      //         }
      //       )
      //     },
      //     div("No vertices")
      //   )


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaAttrs,
        ),
        propTable.laminarElt(
          sema.signal,
          messenger
        ),
        // child <-- cellSig.map(_ match
        //   case cell:PropCell => cell.laminarElt
        //   case elt:Element => elt 
        // )
      )
        
        

      render(mountInto, mainDiv)
    }
  }
}
