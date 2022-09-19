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

/**
 * TODO:
 * - Clean up use of OptionT[Action[PosGraph,_],A]
 * - Dialogue for edge creation
 * - Dialogue for content editing
 * - Content centering and box resizing in response to label
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
 * <bond> element. Then apparently observable/pluto will automatically listen to
 * changes in the value and propagate them back to Julia.
 *
 * I don't think that this supports bidirectional communication, however. Which
 * means that an interactive process would not be supported in the context of
 * Pluto.
 *
 * Notes:
 * The simplest and easiest way to convert ACSets to be displayable might be by
 * just adding a PropMap attribute to each part...
 */

/**
 * A positioned graph
 */
type PosGraph = LabeledGraph[PropMap]

val addBox: Action[PosGraph, Unit] = for {
  pos <- mousePos
  _ <- updateModelS(addLabeledVertex(PropMap() + (Center, pos) + (Content, "squiiid")))
  _ <- update
} yield {}

val remBox: Action[PosGraph, Unit] = (for {
  v <- OptionT(hoveredPart(V))
  _ <- OptionT.liftF(updateModel[PosGraph](_.remPart(v)))
  _ <- OptionT.liftF(update)
} yield {}).value.map(_ => {})


val addEdgeAction: Action[PosGraph, Unit] = (for {
  _ <- OptionT.liftF(mouseDown(MouseButton.LeftButton))
  s <- OptionT(hoveredPart(V))
  _ <- OptionT.liftF(mouseDown(MouseButton.LeftButton))
  t <- OptionT(hoveredPart(V))
  _ <- OptionT.liftF(updateModelS[PosGraph, Elt[E.type]](addEdge(s, t)))
  _ <- OptionT.liftF(update)
} yield {}).value.map(_ => {})

val editVertexText: Action[PosGraph, Unit] = (for {
  v <- OptionT(hoveredPart(V))
  $model <- OptionT.liftF(getModel[PosGraph])
  _ <- OptionT.liftF(
    editText(
      Observer(s =>
        $model.update(m =>
          {
            val p = m.subpart(Label[PropMap](), v).get
            m.setSubpart(Label[PropMap](), v, p + (Content, s))
          }
        )
      ),
      $model.now().subpart(Label[PropMap](), v).get(Content)
    ))
} yield {}).value.map(_ => {})

val bindings = KeyBindings(
  Map(
    "a" -> addBox,
    "d" -> remBox,
    "e" -> addEdgeAction,
    "t" -> editVertexText
  )
)

type M[T] = Action[PosGraph, T]
val L = actionLiftIO[PosGraph]
val A = implicitly[Monad[[X] =>> Action[LabeledGraph[Complex],X]]]

def renderPosGraph(
  $posGraph: Var[PosGraph],
  hover: HoverController,
  drag: DragController
) = {

  val spriteMaps = SpriteMaps[PosGraph](
    $posGraph.signal,
    List(
      SpriteMaker[PosGraph](
        Box(),
        (s, _) => s.parts(V).toList.map(v => (v, s.subpart(Label[PropMap](), v).get)),
        Stack(
          WithDefaults(PropMap()
                         + (MinimumWidth, 40)
                         + (MinimumHeight, 40)
                         + (Fill, "white")
                         + (Stroke, "black")
                         + (InnerSep, 7)
                         + (FontSize, 16)
          ),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
          Draggable.dragPart(drag, $posGraph, Label[PropMap](), Center, MainHandle)
        )
      ),
      SpriteMaker[PosGraph](
        Arrow(),
        (s, propMap) => s.parts(E).toList.map(
          e => {
            val srcEnt = s.subpart(Src, e).get
            val tgtEnt = s.subpart(Tgt, e).get
            val srcCenter = propMap(srcEnt)(Center)
            val tgtCenter = propMap(tgtEnt)(Center)
            val dir = tgtCenter - srcCenter
            val src = Box().boundaryPt(srcEnt, propMap(srcEnt), dir)
            val tgt = Box().boundaryPt(tgtEnt, propMap(tgtEnt), -dir)
            (e, PropMap() + (Start, src) + (End, tgt))
          }
        ),
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

val serializer = labeledGraphACSet[PropMap].rw(
  AttrTypeSerializers() + ATRW(LabelValue[PropMap](), PropMap.rw)
)

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action: M[Unit] = for {
      $model <- ReaderT.ask.map(_.$model)
      hover <- ReaderT.ask.map(_.hover)
      drag <- ReaderT.ask.map(_.drag)
      _ <- addChild(renderPosGraph($model, hover, drag))
      _ <- bindings.runForever
    } yield ()

    mountWithAction(el, LabeledGraph[PropMap](), serializer, action)
  }
}
