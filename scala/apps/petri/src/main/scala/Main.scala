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

import Petris._

type PropPetri = WithProps[Petri]
type M[T] = Action[PropPetri, T]

val bindings = Bindings[PropPetri, Unit](
  keyDown("s").andThen(addEntityPos(S)),
  keyDown("t").andThen(addEntityPos(T)),
  keyDown("d").andThen(remEntity),
  keyDown("Shift").andThen(dragEdge(IS, IT, "Shift")),
  keyDown("Control").andThen(dragEdge(OT, OS, "Control")),
  clickOn(ClickType.Double, MouseButton.Left, S).flatMap(editContent(S)),
  clickOn(ClickType.Single, MouseButton.Left, S).flatMap(dragEntity(S)),
  clickOn(ClickType.Double, MouseButton.Left, T).flatMap(editContent(T)),
  clickOn(ClickType.Single, MouseButton.Left, T).flatMap(dragEntity(T))
)

val L = actionLiftIO[PropPetri]

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
        (s, _) => s.parts(S).toList.map(v => (v, s.subpart(Props(S), v).get)),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "white")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Box(),
        (s, _) => s.parts(T).toList.map(v => (v, s.subpart(Props(T), v).get)),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "white")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
          Clickable(mouse, MainHandle)
        )
      ),
      SpriteMaker[PropPetri](
        Arrow(),
        edgeExtractor(IS, IT),
        Stack(
          WithDefaults(PropMap() + (Stroke, "black")),
          Shorten(5),
          Hoverable(hover, MainHandle, PropMap() + (Stroke, "lightgray"))
        )
      ),
      SpriteMaker[PropPetri](
        Arrow(),
        edgeExtractor(OT, OS),
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

val serializer = withPropsACSet[Petri].rw(
  AttrTypeSerializers() + ATRW(PropValue, PropMap.rw)
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

    mountWithAction(el, WithProps[Petri](), serializer, action)
  }
}
