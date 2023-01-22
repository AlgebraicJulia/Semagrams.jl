package semagrams.wiringdiagrams

import semagrams.api._
import semagrams.acsets.{_, given}
import WiringDiagrams._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

def bindings(es: EditorState, g: Var[ACSet], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("a").andThen(a.addAtMouse(Box, PropMap())),
    keyDown("?").andThen(a.debug),
    keyDown("o").andThen(
      for {
        mb <- es.hoveredPart(PartType(Seq(Box)))
        _ <- IO.println(mb)
        _ <- mb.map(b => a.add(b, OutPort, PropMap())).getOrElse(IO(()))
      } yield ()),
    keyDown("d").andThen(a.del),
    // keyDown("e").andThen(a.importExport),
    // clickOn[SrcPort](MouseButton.Left, SrcPort)
    //   .withMods(KeyModifier.Shift)
    //   .flatMap(a.dragEdge[SrcPort, TgtPort](Wire, Src, Tgt, TgtPort)),
    clickOnPart(MouseButton.Left, PartType(Seq(Box))).withMods().flatMap(a.drag),
  )
}

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      // val initg = init match {
      //   case Some(s) => read[WiringDiagram](s)(ACSet.rw)
      //   case None => WiringDiagram()
      // }
      val initg = WiringDiagram()
      for {
        g <- IO(Var(initg))
        lg <- IO(
          g.signal
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, BasicDPBox(InPort, OutPort)(es)),
            // ACSetEdgeSource(Wire, Src, Tgt, BasicWire(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
