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

val addBox: Action[PropGraph, Unit] = for {
  pos <- mousePos
  _ <- updateModelS[PropGraph, Elt[V.type]](
    addPartWP(V, PropMap() + (Center, pos) + (Content, ""))
  )
  _ <- update
} yield {}

val remBox: Action[PropGraph, Unit] = for {
  v <- fromMaybe(hoveredPart(V))
  _ <- updateModel[PropGraph](_.remPart(v))
  _ <- update
} yield ()

val dragEdge: Action[PropGraph, Unit] = {
  val L = actionLiftIO[PropGraph]
  val mainAction: Action[PropGraph, Unit] = for {
    drag <- Kleisli.ask.map(_.drag)
    $model <- Kleisli.ask.map(_.$model)
    s <- fromMaybe(Bindings(clickOn(ClickType.Single, MouseButton.Left, V)).run)
    p <- mousePos
    e <- updateModelS[PropGraph, Elt[E.type]](for {
      e <- addPartWP(E, PropMap())
      _ <- setSubpart(Src, e, s)
      _ <- setProp(e, End, p)
    } yield e)
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setProp(e, End, p))))
      t <- fromMaybe(hoveredPart(V))
      _ <- updateModelS[PropGraph, Unit](setSubpart(Tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- L.liftIO(IO(drag.$state.set(None)))
      _ <- updateModelS[PropGraph, Unit](remPart(e))
    } yield ())
    _ <- update
  } yield ()

  for {
    drag <- Kleisli.ask.map(_.drag)
    target <- mainAction.forever.start
    _ <- Bindings(keyUp("Shift")).run
    _ <- Kleisli(_ => target.cancel)
  } yield {}
}

def editVertexText(v: Elt[V.type]): Action[PropGraph, Unit] = for {
  $model <- getModel[PropGraph]
  _ <- editText(
    Observer(s => $model.update(m => { m.setProp(v, Content, s) })),
    $model.now().getProp(v, Content)
  )
} yield {}

def dragVertex(v: Elt[V.type]): Action[PropGraph, Unit] = for {
  $model <- getModel
  c <- Kleisli.pure($model.now().getProp(v, Center))
  init <- mousePos
  offset <- Kleisli.pure(c - init)
  drag <- Kleisli.ask.map(_.drag)
  _ <- drag.dragStart(
    Observer(p => $model.update(_.setProp(v, Center, p + offset)))
  )
} yield ()

val bindings = Bindings(
  keyDown("a").andThen(addBox),
  keyDown("d").andThen(remBox),
  keyDown("Shift").andThen(dragEdge),
  clickOn(ClickType.Double, MouseButton.Left, V).flatMap(editVertexText),
  clickOn(ClickType.Single, MouseButton.Left, V).flatMap(dragVertex)
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
          Shorten(5)
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
