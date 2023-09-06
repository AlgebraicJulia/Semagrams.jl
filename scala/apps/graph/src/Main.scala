package semagrams.graph

import semagrams._
import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.listeners._
import semagrams.widgets._

import Graphs._

import upickle.default._
import com.raquo.laminar.api.L._
import com.raquo.laminar.codecs.StringAsIsCodec

import cats._
import cats.syntax._
import cats.effect._
import cats.effect.std._

import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js

object GraphDisplay extends ACSemagram {
  type Model = ACSet

  def layout(g: ACSet) = assignBends(Map(E -> (Src, Tgt)), 0.5)(g)

  val entitySources = Seq(
    ACSetEntitySource(V, Disc()),
    ACSetEdgeSource(E, Src, Tgt, Arrow()),
  )

  val schema: Schema = SchGraph
}

val bindings = Seq[Binding[ACSet]](
  Binding(KeyDownHook("a"), AddAtMouse(V)),
  Binding(KeyDownHook("d"), DeleteHovered()),
  Binding(ClickOnPartHook(MouseButton.Left, Set(KeyModifier.Shift)), AddEdgeViaDrag(E, Src, Tgt)),
  Binding(ClickOnPartHook(MouseButton.Left), MoveViaDrag()),
  Binding(MsgHook(),ProcessMsg()),
  Binding(DoubleClickOnPartHook(),PartAction(
    part => SetSubpartMsg(part.asInstanceOf[Part],Editing)(())
  )),
  Binding(KeyDownHook("?"), PrintModel)
)


object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {

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
      val dims = Var((1,1))
      val rand = scala.util.Random()

      val propTable = PropTable(V,Seq(Content,Center,Fill))


      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaAttrs,
        ),
        button(
          "Click Me",
          onClick.mapTo {
            dims.update(_ => (sema.elt.ref.offsetWidth.toInt,sema.elt.ref.offsetHeight.toInt))

            val props = PropMap() + (Center,Complex(
              rand.nextInt(dims.now()._1),
              rand.nextInt(dims.now()._2),
            ))

            AddPartMsg(V, props)
          } --> messenger
        ),
        propTable.laminarElt(
          sema.signal,
          messenger
        )
        // PropTable(Seq(Content,Center,Fill)).laminarElt(
        //   V,
        //   sema.signal,
        //   messenger
        // )
      )
      render(mountInto, mainDiv)
    }
  }
}
