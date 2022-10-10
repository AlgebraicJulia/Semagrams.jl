package graph

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

/** TODO:
  *   - Clean up use of OptionT[Action[PosGraph,_],A]
  *   - Dialogue for edge creation
  *   - Dialogue for content editing
  *   - Content centering and box resizing in response to label
  *
  * Pluto integration notes.
  *
  * The good news is that Pluto handles all the connection stuff. The handoff
  * point to Pluto happens within javascript, so we don't have to do anything
  * with websockets in Semagrams.
  *
  * The first thing to do is to set up something which gets the absolute rawest
  * data into Julia. We can process this later.
  *
  * Essentially, the way to integrate is to display a script element from Julia
  * which updates the property "value" on its parent, and then wrap this in a
  * <bond> element. Then apparently observable/pluto will automatically listen
  * to changes in the value and propagate them back to Julia.
  *
  * I don't think that this supports bidirectional communication, however. Which
  * means that an interactive process would not be supported in the context of
  * Pluto.
  *
  * Notes: The simplest and easiest way to convert ACSets to be displayable
  * might be by just adding a PropMap attribute to each part...
  */

/** A positioned graph
  */
type PropGraph = WithProps[Graph]
type M[T] = Action[PropGraph, T]

val bindings = Bindings[PropGraph, Unit](
  keyDown("a").andThen(addEntityPos(V)),
  keyDown("d").andThen(remEntity),
  keyDown("Shift").andThen(dragEdge(Src, Tgt)),
  clickOn(ClickType.Double, MouseButton.Left, V).flatMap(editContent(V)),
  clickOn(ClickType.Single, MouseButton.Left, V).flatMap(dragEntity(V))
)

val L = actionLiftIO[PropGraph]

def renderPosGraph(
    $posGraph: Var[PropGraph],
    hover: HoverController,
    drag: DragController,
    mouse: MouseController
) = {

  val spriteMaps = SpriteMaps[PropGraph](
    $posGraph.signal,
    List(
      SpriteMaker[PropGraph](
        Disc(),
        (s, _) => s.parts(V).toList.map(v => (v, s.subpart(Props(V), v).get)),
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
      SpriteMaker[PropGraph](
        Arrow(),
        edgeExtractor(Src, Tgt),
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

val serializer = withPropsACSet[Graph].rw(
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
      _ <- addChild(renderPosGraph($model, hover, drag, mouse))
      _ <- bindings.runForever
    } yield ()

    mountWithAction(el, WithProps[Graph](), serializer, action)
  }
}
