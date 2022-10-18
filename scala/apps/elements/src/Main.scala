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
import scala.scalajs.js

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

val bindings: Bindings[DynACSet, Unit] = Bindings(
  clickOnPart(ClickType.Single, MouseButton.Left).flatMap(dragPart),

)

val serializer = ACSet.rw[DynSchema]

sealed trait Command

object Command {
  case class Schema(data: ujson.Value) extends Command
  object Schema {
    implicit val rw: ReadWriter[Schema] = macroRW
  }
  case class ACSet(
      data: Map[String, Seq[Map[String, ujson.Value]]],
      positions: Seq[((String, Int), Complex)]
  ) extends Command
  object ACSet {
    implicit val rw: ReadWriter[ACSet] = macroRW
  }
}

given ReadWriter[Command] = macroRW

val scale = 1.5
val offset = Complex(20, 40)

def handleCommand(state: DynACSet, c: Command) = {
  c match {
    case Command.Schema(data) => {
      val schema = read[DynSchema](data)
      DynACSet(schema)
    }
    case Command.ACSet(data, positions) => {
      val (acs, partMap) = state.schema.readACSet(data)
      val posProps = positions.map({ case ((s, i), p) =>
        (partMap((DynOb(s), i)), PropMap() + (Center, p * scale + offset) + (Content, s"$s: $i"))
      })
      acs.setSubparts(posProps)
    }
  }
}

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val commandBus = EventBus[Command]()

    val callback: js.Function1[String, Unit] = (x: String) => {
      commandBus.emit(read(x))
    }

    js.Dynamic.global.window.parse_payload = callback

    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- addChild(renderElements($model, hover, mouse))
      _ <- amendElt(commandBus.events --> $model.updater(handleCommand))
      _ <- bindings.runForever
    } yield ()

    plutoMain(el, DynACSet(DynSchema()), serializer, action)
  }
}
