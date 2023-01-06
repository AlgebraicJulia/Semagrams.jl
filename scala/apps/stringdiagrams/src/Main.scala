package semagrams.stringdiagrams

import semagrams.api._
import semagrams.acsets.{_, given}
import WiringDiagrams._

import upickle.default._
import com.raquo.laminar.api.L._
import cats.effect._
import scala.scalajs.js.annotation.JSExportTopLevel

def bindings(es: EditorState, g: Var[WiringDiagram], ui: UIState) = {
  val a = Actions(es, g, ui)

  Seq(
    keyDown("a").andThen(a.add(Box, PropMap())),
    keyDown("d").andThen(a.del),
    keyDown("e").andThen(a.importExport),
    keyDown("i").andThen(for {
                           mb <- es.hoveredPart(Box)
                           _ <- mb.map(b => a.add(InPort, PropMap().set(InParent, b))).getOrElse(IO(()))
                         } yield ()),
    keyDown("o").andThen(for {
                           mb <- es.hoveredPart(Box)
                           _ <- mb.map(b => a.add(OutPort, PropMap().set(OutParent, b))).getOrElse(IO(()))
                         } yield ()),
    clickOnPart(MouseButton.Left, OutPort)
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge(Wire, Src, Tgt)),
    clickOnPart(MouseButton.Left, Box).withMods().flatMap(a.drag),
  )
}

def portSource[S: IsSchema](ob: Ob, parent: Hom, sprite: Sprite, dir: Double) = {
  ACSetEntitySource[S](ob, sprite).addPropsBy(
    (e,p,entities) => {
      val (parentSprite, parentProps) = entities(p(parent))
      val c = parentSprite.boundaryPt(parentProps, Complex(dir, 0))
        + Complex(0, parentSprite.bbox(parentProps).dims.y / 2) * Complex(p(RelPos),0)
      p + (Center, c)
    }
  )
}

def PortSprite(es: EditorState) =
  BasicWrapper(PropMap() + (Fill, "lightgrey"))(
    Disc(PropMap() + (MinimumHeight, 5) + (MinimumWidth, 5))
  )(es)

object Main {
  @JSExportTopLevel("App")
  object App extends Semagram {

    def run(es: EditorState, init: Option[String]): IO[Unit] = {
      val initg = init match {
        case Some(s) => read[WiringDiagram](s)(ACSet.rw)
        case None => WiringDiagram()
      }
      for {
        g <- IO(Var(initg))
        lg <- IO(
          g.signal
            .map(FixedRangeExceptEnds(-1,1).assignPositions(InPort, InParent, RelPos))
            .map(FixedRangeExceptEnds(-1,1).assignPositions(OutPort, OutParent, RelPos))
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, BasicRect(es)),
            portSource[SchWiringDiagram.type](InPort, InParent, PortSprite(es), -1),
            portSource[SchWiringDiagram.type](OutPort, OutParent, PortSprite(es), 1),
            ACSetEdgeSource(Wire, Src, Tgt, BasicArrow(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
