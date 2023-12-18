package semagrams.elements

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.actions._
import semagrams.controllers._
import semagrams.sprites._
import semagrams.util._
import semagrams.widgets._

import cats.effect.syntax.all._

import upickle.default._
import com.raquo.laminar.api.L._

import org.scalajs.dom
import scala.scalajs.js

import scala.scalajs.js.annotation.JSExportTopLevel

val ops = Action.ops[DynACSet]

val dims = Complex(1550, 800)

case class HomArrow(
  src: Part,
  tgt: Either[Complex, Part],
  hom: Hom
) extends Entity

def arrowExtractor(d: DynACSet, sprites: Sprites): List[(Entity, PropMap)] = {
  d.props
    .map((e, pm) => {
      d.schema.allhoms
        .filter(_.dom == e.ob)
        .collect({
          case f if pm contains f =>
            (
              HomArrow(e, Right(pm(f)), f),
              spanProps(
                sprites,
                Some(e),
                sprites(e)._2(Center),
                Some(pm(f)),
                sprites(pm(f))._2(Center),
                0
              ) ++
                (if (f.name.contains("_is_")) then
                   PropMap() + (StrokeDasharray, "3 3")
                 else PropMap())
            )
        })
    })
    .flatten
    .toList
}

val cellS = Seq(
  border := "1px solid black",
  borderCollapse := "collapse",
  padding := "10px"
)

def attributeTable(p: Part, acs: DynACSet, eltDims: Complex): SvgElement = {
  val pos = Complex(dims.x / 2, dims.y)
    - Complex(eltDims.x / 2, eltDims.y)

  val borderV = 1

  val s = acs.schema
  wrappedHtml(
    table(
      backgroundColor := "white",
      position := "absolute",
      textAlign := "center",
      bottom := "5",
      cellS,
      tr(th(cellS, "Attribute"), th(cellS, "Value")),
      s.attrs(p.ob)
        .map(a =>
          tr(
            td(cellS, a.asInstanceOf[DynAttr].name),
            td(cellS, acs.subpart(a, p).toString())
          )
        )
    ),
    pos,
    eltDims
  )
}

case object AttributeTable extends ElementHandle

def showAttributes(p: Part): Action[DynACSet, Unit] = for {
  $m <- ops.ask.map(_.$model)
  m <- ops.delay($m.now())
  tab <- ops.delay(attributeTable(p, m, Complex(400, 300)))
  _ <- addControlElt(AttributeTable, tab)
  _ <- (for {
    _ <- Bindings[DynACSet, Unit](
      keyDown("Escape"),
      mouseDown(MouseButton.Left).mapTo(())
    ).run
    _ <- removeControlElt(AttributeTable)
  } yield ()).start
} yield ()

def renderElements(
    $d: Var[DynACSet],
    hover: HoverController,
    mouse: MouseController,
    sceneElements: Var[Map[ElementHandle, Element]]
) = {
  val spriteMaps = SpriteMaps(
    $d.signal,
    List(
      SpriteMaker[DynACSet](
        Box(),
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
          Hoverable(hover, MainHandle, PropMap() + (Stroke, "lightgray")),
          Tooltip(
            MainHandle,
            ent => ent.asInstanceOf[HomArrow].hom.asInstanceOf[DynHom].name,
            mouse,
            sceneElements
          )
        )
      )
    )
  )

  svg.g(
    spriteMaps.attach
  )
}

val zoomFactor = 1.1

val bindings = Bindings[DynACSet, Unit](
  keyDown("+").andThen(zoomAtMouse(zoomFactor)),
  keyDown("-").andThen(zoomAtMouse(1 / zoomFactor)),
  clickOn(ClickType.Single, MouseButton.Left, BackgroundType).andThen(dragPan),
  clickOnPart(ClickType.Single, MouseButton.Left).flatMap(dragPart),
  clickOnPart(ClickType.Double, MouseButton.Left).flatMap(showAttributes)
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
        (
          partMap((DynOb(s), i)),
          PropMap() + (Center, p * scale + offset) + (Content, s"$s: $i")
        )
      })
      acs.setProps(posProps)
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
      sceneElements <- ops.ask.map(_.sceneElements)
      _ <- addSceneElt(renderElements($model, hover, mouse, sceneElements))
      _ <- amendElt(commandBus.events --> $model.updater(handleCommand))
      _ <- bindings.runForever
    } yield ()

    // val schLabeledDDS = read[DynSchema](Data.schLabeledDDS)
    // val (dds, _) = schLabeledDDS.readACSet(Data.exLabeledDDS)

    plutoMain(el, DynACSet(DynSchema()), serializer, action, dims)
  }
}
