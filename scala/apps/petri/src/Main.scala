package semagrams.petri

import semagrams._
import semagrams.util._
import semagrams.acsets.{*, given}
import semagrams.actions._
import semagrams.text._
import cats.data._
import cats.Monad
import cats.effect.IO
import cats.data.OptionT
import semagrams.sprites._
import com.raquo.laminar.api.L.{*, given}
import semagrams.controllers._
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import upickle.default._

import Petris._
import scala.collection.MapView.Keys

type PropPetri = WithProps[LabelledReactionNet]
type M[T] = Action[PropPetri, T]

val ops = Action.ops[PropPetri]

val addSpecies = addEntityPos[LabelledReactionNet, S.type](
  S,
  v =>
    for {
      _ <- setSubpart[PropPetri, S.type, String](SName, v, "")
      _ <- setSubpart[PropPetri, S.type, Double](Concentration, v, 1.0)
    } yield ()
)

val addTransition = addEntityPos[LabelledReactionNet, T.type](
  T,
  v =>
    for {
      _ <- setSubpart[PropPetri, T.type, String](TName, v, "")
      _ <- setSubpart[PropPetri, T.type, Double](Rate, v, 1.0)
    } yield ()
)


val helpText = """
Keys:
s: add species
t: add transition
d: delete hovered species/transition/arrow
h: toggle quick help
p: tutorial

Click and drag to move around species/transitions

Double-click to modify labels

Shift-click and drag to add arrows

Control-click and drag to modify rates/concentrations
""".linesIterator.toSeq

val bindings = Bindings[PropPetri, Unit](
  keyDown("s").andThen(addSpecies.flatMap(editStringAttr(S, SName))),
  keyDown("t").andThen(addTransition.flatMap(editStringAttr(T, TName))),
  keyDown("d").andThen(remEntity),
  keyDown("h").andThen(showPopoverUntil(helpText, keyDown("h"))),
  clickOn(ClickType.Single, MouseButton.Left, S)
    .withMods(KeyModifier.Shift)
    .flatMap(s => dragEdge[LabelledReactionNet, I.type, S.type, T.type](IS, IT, s)),
  clickOn(ClickType.Single, MouseButton.Left, T)
    .withMods(KeyModifier.Shift)
    .flatMap(t => dragEdge[LabelledReactionNet, O.type, T.type, S.type](OT, OS, t)),
  clickOn(ClickType.Double, MouseButton.Left, S)
    .flatMap(editStringAttr(S, SName)),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
  clickOn(ClickType.Double, MouseButton.Left, T)
    .flatMap(editStringAttr(T, TName)),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
)

def arcExtractor(p: PropPetri, sprites: Sprites) = {
  val bends = assignBends(List((I, IS, IT, 1), (O, OS, OT, -1)), p, 0.3)
  val inputs = edgeExtractor(IS, IT)(p, sprites, bends)
  val outputs = edgeExtractor(OT, OS)(p, sprites, bends)
  inputs ++ outputs
}

def renderPetri(
    $petri: Var[PropPetri],
    hover: HoverController,
    drag: DragController,
    mouse: MouseController
) = {

  val spriteMaps = SpriteMaps[PropPetri](
    $petri.signal,
    List(
      SpriteMaker[PropPetri](
        Disc(),
        (s, _) =>
          s.parts(S)
            .toList
            .map(v =>
              (
                v,
                s.subpart(Props(S), v).get + (Content, s.subpart(SName, v).getOrElse(""))
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#6C9AC3")
              + (Stroke, "none")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "#97b7d4")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Box(),
        (s, _) =>
          s.parts(T)
            .toList
            .map(v =>
              (
                v,
                s.subpart(Props(T), v).get
                  + (Content, s .subpart(TName, v).getOrElse(""))
                  + (Pulse, s.subpart(Rate, v).get)
              )
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#E28F41")
              + (Stroke, "none")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "#eaaf78")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Arrow(),
        arcExtractor,
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

val serializer = withPropsACSet[LabelledReactionNet].rw(
  AttrTypeSerializers()
    + ATRW(PropValue, PropMap.rw)
    + ATRW(NameValue, summon[ReadWriter[String]])
    + ATRW(RateValue, summon[ReadWriter[Double]])
    + ATRW(ConcentrationValue, summon[ReadWriter[Double]])
)

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action: M[Unit] = for {
      $model <- ReaderT.ask.map(_.$model)
      hover <- ReaderT.ask.map(_.hover)
      drag <- ReaderT.ask.map(_.drag)
      mouse <- ReaderT.ask.map(_.mouse)
      _ <- addChild(renderPetri($model, hover, drag, mouse))
      _ <- bindings.runForever
    } yield ()

    mountWithAction(el, WithProps[LabelledReactionNet](), serializer, action)
  }
}
