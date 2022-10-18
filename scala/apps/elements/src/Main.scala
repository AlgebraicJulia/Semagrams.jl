package semagrams.elements

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.actions._
import semagrams.controllers._
import semagrams.sprites._
import semagrams.util._

import upickle.default._
import com.raquo.laminar.api.L._

import org.scalajs.dom

import scala.scalajs.js.annotation.JSExportTopLevel

val ops = Action.ops[DynACSet]

object HomArrowType extends EntityType

case class HomArrow(src: Part, tgt: Either[Complex, Part], hom: Hom)
    extends Entity {
  val entityType = HomArrowType

  val hash = (src.hash, tgt.hashCode(), hom.toString()).hashCode()
}

def arrowExtractor(d: DynACSet, sprites: Sprites): List[(Entity, PropMap)] = {
  d.props
    .map((e, pm) => {
      d.schema.allhoms
        .filter(_.dom == e.ob)
        .collect({
          case f if pm contains f =>
            (
              HomArrow(e, Right(pm(f)), f),
              edgeProps(
                sprites,
                Some(e),
                sprites(e)._2(Center),
                Some(pm(f)),
                sprites(pm(f))._2(Center),
                0
              )
            )
        })
    })
    .flatten
    .toList
}

def renderElements(
    $d: Var[DynACSet],
    hover: HoverController,
    mouse: MouseController
) = {
  val spriteMaps = SpriteMaps(
    $d.signal,
    List(
      SpriteMaker[DynACSet](
        Disc(),
        (d, _) => d.props.toList,
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#6C9AC3")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(
            hover,
            MainHandle,
            PropMap() + (Style, "filter: opacity(0.7)")
          ),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[DynACSet](
        Arrow(),
        arrowExtractor,
        Stack(
          WithDefaults(PropMap() + (Stroke, "black")),
          Shorten(5),
          Hoverable(hover, MainHandle, PropMap() + (Stroke, "lightgray"))
        )
      )
    )
  )

  svg.g(
    spriteMaps.attach
  )
}

val bindings: Bindings[DynACSet, Unit] = Bindings()

val serializer = ACSet.rw[DynamicSchema]

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- addChild(renderElements($model, hover, mouse))
      _ <- bindings.runForever
    } yield ()

    val sch = read[DynamicSchema](TestData.schLabeledDDS)
    val dds = sch.readACSet(TestData.exLabeledDDS).setSubparts(TestData.props)

    plutoMain(el, dds, serializer, action)
  }
}
