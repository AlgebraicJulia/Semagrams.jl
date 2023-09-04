package semagrams.graph

import semagrams.api._
import semagrams.acsets._
import semagrams.bindings._
import semagrams.listeners._
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
import semagrams.ACSemagram
import semagrams.acsets.ACSet.AddPartMsg
import semagrams.SemagramElt
import widgets._

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
  Binding(MsgHook(),ProcessMsg())
)

class DisplayProperty(val f: Property, val serializer: f.Value => String)





object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      // SemagramElt[ACSet] ≅ Element × Readout × Update
      val sema = GraphDisplay(bindings)

      // Check that it's the same element
      println(sema.elt.toString())      



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


      val tickStream = EventStream.periodic(1000)
      
      val dims = Var((1,1))
      val rand = scala.util.Random()


      val s = Var("Hi")

      val clickBus = new EventBus[Unit]

      val mainDiv: Div = div( 
        idAttr := "mainDiv",
        sema.elt.amend(
          semaAttrs,
        ),
        button(
          "Click Me",
          onClick.mapTo({
            dims.update(_ => (sema.elt.ref.offsetWidth.toInt,sema.elt.ref.offsetHeight.toInt))
            ACSet.AddPartMsg(
              V,PropMap() + (Center,Complex(
                rand.nextInt(dims.now()._1),
                rand.nextInt(dims.now()._2),
              ))
            )
          }) --> messenger
        ),
        PropTable(Seq(Center)).laminarElt(
          V,
          sema.signal,
          sema.observer
        )

      )

      // mainDiv.amend(
      //   button(
      //     "add row",
      //     onClick --> Observer(
      //       _ => mainDiv.amend(

      //       )
      //     )
      //   )
      // )
        // child <-- tickStream.map(_ => 
        //   // DTable(sema.readout(),V).render()
        // )
      

      render(mountInto, mainDiv)



    }
  }
}
