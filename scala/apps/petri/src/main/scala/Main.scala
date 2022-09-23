package petri

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

type PropPetri = WithProps[LabelledReactionNet]
type M[T] = Action[PropPetri, T]

val arcLoop: Action[PropPetri, Unit] =
  loopDuringPress(
    "Shift",
    for {
      v <- fromMaybe(
        Bindings(
          clickOn(ClickType.Single, MouseButton.Left, S).map(Left(_)),
          clickOn(ClickType.Single, MouseButton.Left, T).map(Right(_))
        ).run
      )
      _ <- v match {
        case Left(s) =>
          dragEdge[LabelledReactionNet, I.type, S.type, T.type](IS, IT, s)
        case Right(t) =>
          dragEdge[LabelledReactionNet, O.type, T.type, S.type](OT, OS, t)
      }
    } yield {}
  )

val modifyRates: Action[PropPetri, Unit] =
  loopDuringPress(
    "Control",
    for {
      v <- fromMaybe(
        Bindings(
          clickOn(ClickType.Single, MouseButton.Left, S).map(Left(_)),
          clickOn(ClickType.Single, MouseButton.Left, T).map(Right(_))
        ).run
      )
      _ <- v match {
        case Left(s)  => dragControl[PropPetri, S.type](Concentration, 0.1)(s)
        case Right(t) => dragControl[PropPetri, T.type](Rate, 0.1)(t)
      }
    } yield {}
  )

val addSpecies = addEntityPos[LabelledReactionNet, S.type](
  S,
  v =>
    for {
      _ <- setSubpart(SName, v, "")
      _ <- setSubpart(Concentration, v, 1.0)
    } yield ()
).flatMap(editStringAttr(S, SName))

val addTransition = addEntityPos[LabelledReactionNet, T.type](
  T,
  v =>
    for {
      _ <- setSubpart(TName, v, "")
      _ <- setSubpart(Rate, v, 1.0)
    } yield ()
).flatMap(editStringAttr(T, TName))

val bindings = Bindings[PropPetri, Unit](
  keyDown("s").andThen(addSpecies),
  keyDown("t").andThen(addTransition),
  keyDown("d").andThen(remEntity),
  keyDown("Shift").andThen(arcLoop),
  keyDown("Control").andThen(modifyRates),
  clickOn(ClickType.Double, MouseButton.Left, S)
    .flatMap(editStringAttr(S, SName)),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
  clickOn(ClickType.Double, MouseButton.Left, T)
    .flatMap(editStringAttr(T, TName)),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
)

val L = actionLiftIO[PropPetri]

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
                s.subpart(Props(S), v).get + (Content, s
                  .subpart(SName, v)
                  .getOrElse("") + "," + "%.1f"
                  .format(s.subpart(Concentration, v).get))
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
                s.subpart(Props(T), v).get + (Content, s
                  .subpart(TName, v)
                  .getOrElse("") + "," + "%.1f".format(s.subpart(Rate, v).get))
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
