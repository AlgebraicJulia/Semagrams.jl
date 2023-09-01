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





case class DTable(_cols:Seq[DColumn],_key:Option[DColumn] = None) {

  val cols = _key match
    case None => _cols
    case Some(keycol) => keycol +: _cols.filter(_!=keycol)
  
  val keycol = _key match
    case None => _cols.head
    case Some(k) => k
  
  def headerRow = tr(cols.map(col => th(col.header)))
  
  def rows = (0 until keycol.values.length) map( idx => tr(
    cols.map(col =>
      td(col.values(idx).toString())
    )
  ))

  def render() = table(
    headerRow,
    rows
  )

    
}

object DTable {
  def apply(a:ACSet,ob:Ob): DTable = DTable(Seq(
    DCol("id",a.partsOnly(ROOT,ob).map(partid)),
    DCol("ctr",a.parts(ROOT,ob).map(_._2.props(Center)))
  ))
}

trait DColumn {
  val header: String
  val values: Seq[Any]
}

case class DCol[T](header:String,values:Seq[T]) extends DColumn





case class TableCell[T:ReadWriter](value:T) {
  var editing: Boolean = false


}




def partid(p: Part) = p.path(0)._2.id.toString



object Main {
  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(mountInto: dom.Element, init: js.UndefOr[String]) = {


      // SemagramElt[ACSet] ≅ Element × Readout × Update
      val sema = GraphDisplay(bindings)

      // Check that it's the same element
      println(sema.elt.toString())      

      // It errors here
      sema.elt.amend(
        // backgroundColor := "green"
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

      val messenger = Observer(
        (m:Message[ACSet]) => sema.update(a => m.execute(a))
      )


      val tickStream = EventStream.periodic(1000)
      



      val mainDiv = div(
        idAttr := "mainDiv",
        sema.elt,
        button(
          "Click Me",
          onClick.mapTo(
            ACSet.AddPartMsg(
              V,PropMap() + (Center,Complex(
                100,
                100
              ))
            )
            // "How do I send a message?"
          ) --> messenger
        ),
        child <-- tickStream.map(_ => 
          DTable(sema.readout(),V).render()
        )
      )

      render(mountInto, mainDiv)


    }
  }
}
