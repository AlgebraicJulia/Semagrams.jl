package semagrams.graph

import semagrams.api._
import semagrams.acsets._
import Graphs._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import scala.scalajs.js.annotation._
import scala.scalajs.js

// case class LabelFor(e: Entity) extends Entity

// def bindings(es: EditorState, g: Var[Graph], ui: UIState) = {
//   val a = Actions(es, g, ui)

//   Seq(
//     keyDown("a").andThen(a.add_(V, PropMap().set(MinimumWidth, 60).set(MinimumHeight, 60))),
//     keyDown("d").andThen(a.del),
//     keyDown("e").andThen(a.importExport),
//     keyDown("l").andThen(
//       for {
//         mv <- es.hoveredPart(V)
//         _ <- mv.map(v => a.edit(Label, false)(v)).getOrElse(IO(()))
//       } yield ()),
//     clickOnPart(MouseButton.Left, V).withMods().flatMap(a.dragMove),
//     clickOnPart(MouseButton.Left, V)
//       .withMods(KeyModifier.Shift)
//       .flatMap(a.dragEdge(E, Src, Tgt)),
//     dblClickOnPart(MouseButton.Left, V).withMods().flatMap(a.edit(ImageURL, false)),
//   )
// }

object GraphDisplay extends Semagram {
  type Model = ACSet

  def layout(g: ACSet) = g

  def produceSprites(g: ACSet, eventWriter: Observer[Event]) = {
    EntityCollector.collect(
      g,
      Seq(
        ACSetEntitySource(V, Disc()),
        ACSetEdgeSource(E, Src, Tgt, Arrow()),
      )
    )
  }
}

object Main {
  // TODO:
  //
  // - Bindings
  // - Mouse position
  // - Hovering

  @JSExportTopLevel("GraphApp")
  object GraphApp {
    @JSExport
    def main(div: dom.Element, init: js.UndefOr[String]) = {
      val m = Var(Graph())
      val display = GraphDisplay(m.signal, Observer(dom.console.log(_))).amend(
        svg.height := "100%",
        svg.width := "100%",
        svg.style := "border: black; border-style: solid; background-color: white; box-sizing: border-box"
      )
      render(div, display)
      m.update(_.addPart(V, PropMap() + (Center -> Complex(100, 100)))._1)
    }
  }

  // @JSExportTopLevel("GraphApp")
  // object GraphApp extends Semagram {

  //   def run(es: EditorState, init: Option[String]): IO[Unit] = {
  //     val initg = init match {
  //       case Some(s) => read[Graph](s)(ACSet.rw)
  //       case None => Graph()
  //     }
  //     val LabelBox = Box(
  //       PropMap()
  //         + (MinimumHeight, 8)
  //         + (MinimumWidth, 8)
  //         + (InnerSep, 0)
  //         + (OuterSep, 5)
  //     )
  //     for {
  //       g <- IO(Var(initg))
  //       lg <- IO(
  //         g.signal.map(assignBends[SchGraph.type](Map(E -> (Src, Tgt)), 0.35))
  //       )
  //       _ <- es.makeViewport(
  //         lg,
  //         Seq(
  //           ACSetEntitySource(V, BasicDisc(es)),
  //           ACSetEntitySource[SchGraph.type](V, LabelBox).updateEntities(
  //             (e,p) => (
  //               LabelFor(e),
  //               PropMap()
  //                 + (Center, p(Center) + Complex(0,-70))
  //                 + (Content, p.get(Label).getOrElse(""))
  //                 + (Stroke, "none")
  //             )
  //           ),
  //           ACSetEdgeSource(E, Src, Tgt, BasicArrow(es))
  //         )
  //       )
  //       ui <- es.makeUI()
  //       _ <- es.bindForever(bindings(es, g, ui))
  //     } yield ()
  //   }
  // }
}
