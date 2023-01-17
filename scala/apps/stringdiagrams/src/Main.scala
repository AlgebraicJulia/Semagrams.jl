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
    keyDown("a").andThen(a.add(Box, PropMap().set(boxTy, BoxType(Seq((),()), Seq(()))))),
    keyDown("d").andThen(a.del),
    keyDown("e").andThen(a.importExport),
    clickOn[SrcPort](MouseButton.Left, SrcPort)
      .withMods(KeyModifier.Shift)
      .flatMap(a.dragEdge[SrcPort, TgtPort](Wire, Src, Tgt, TgtPort)),
    clickOnPart(MouseButton.Left, Box).withMods().flatMap(a.drag),
  )
}

// def portSource[S: IsSchema](ob: Ob, parent: Hom, sprite: Sprite, dir: Double) = {
//   ACSetEntitySource[S](ob, sprite).addPropsBy(
//     (e,p,entities) => {
//       val (parentSprite, parentProps) = entities(p(parent))
//       val c = parentSprite.boundaryPt(parentProps, Complex(dir, 0))
//         + Complex(0, parentSprite.bbox(parentProps).dims.y / 2) * Complex(p(RelPos),0)
//       p + (Center, c)
//     }
//   )
// }

def PortSprite(dir: Complex, es: EditorState) =
  BasicWrapper(PropMap() + (Stroke, "lightgrey"))(
    WireStub(PropMap() + (Stroke, "black"), dir)
  )(es)

def spacePorts
  (entityMap: EntityMap, parent: Entity)(portEntities: Seq[Entity], sprite: Sprite, dir: Double) =
{
  val (parentSprite, parentProps) = entityMap(parent)
  val spacer = FixedRangeExceptEnds(-1,1)
  portEntities.zipWithIndex.map(
    (e,i) => {
      val c = parentSprite.boundaryPt(MainHandle, parentProps, Complex(dir, 0))
      + Complex(0, parentSprite.bbox(MainHandle, parentProps).dims.y / 2) * spacer.assignPos(i, portEntities.length)
      (e, sprite, PropMap() + (Center, c))
    })
}

def PortSource(inSprite: Sprite, outSprite: Sprite) = EntitySource[WiringDiagram]((acs, m) =>
    acs.parts(Box).flatMap(
      b => {
        val ty = acs.subpart(boxTy, b)
        val spacer = spacePorts(m, b)
        spacer(ty.inports.zipWithIndex.map((_, i) => TgtPort.Box(b.id, i)), inSprite, -1)
        ++ spacer(ty.outports.zipWithIndex.map((_, i) => SrcPort.Box(b.id, i)), outSprite, 1)
      })
  )

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
        )
        _ <- es.makeViewport(
          lg,
          Seq(
            ACSetEntitySource(Box, BasicRect(es)),
            PortSource(PortSprite(-10, es), PortSprite(10, es)),
            ACSetEdgeSource(Wire, Src, Tgt, BasicWire(es))
          )
        )
        ui <- es.makeUI()
        _ <- es.bindForever(bindings(es, g, ui))
      } yield ()
    }
  }
}
