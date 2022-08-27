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

/**
 * TODO:
 * - Clean up use of OptionT[Action[PosGraph,_],A]
 * - Prettier edges
 * - Dialogue for edge creation
 * - Dialogue for content editing
 * - Content centering and box resizing in response to label
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
  _ <- updateModelS(addLabeledVertex(PropMap() + (Center, pos) + (Content, "Hi")))
} yield {}

val remBox: Action[PosGraph, Unit] = (for {
  v <- OptionT(hoveredPart[PosGraph, V.type])
  _ <- OptionT.liftF(updateModel[PosGraph](_.remPart(v)))
} yield {}).value.map(_ => {})

val addEdgeAction: Action[PosGraph, Unit] = (for {
  _ <- OptionT.liftF(mouseDown(MouseButton.LeftButton))
  s <- OptionT(hoveredPart[PosGraph, V.type])
  _ <- OptionT.liftF(mouseDown(MouseButton.LeftButton))
  t <- OptionT(hoveredPart[PosGraph, V.type])
  _ <- OptionT.liftF(updateModelS[PosGraph, Elt[E.type]](addEdge(s, t)))
} yield {}).value.map(_ => {})

val bindings = KeyBindings(
  Map(
    "a" -> addBox,
    "d" -> remBox,
    "e" -> addEdgeAction
  )
)

type M[T] = Action[PosGraph, T]
val L = actionLiftIO[PosGraph]
val A = implicitly[Monad[[X] =>> Action[LabeledGraph[Complex],X]]]

extension(b: PosGraph)
  def labeledVertices() = {
    val vs = b.vertices().toList
    vs.map(v => (v, b.subpart(Label[Complex], v).get))
  }

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
        (s, _) => s.parts(V).toList.map(v => (v, s.subpart(Label[PropMap], v).get)),
        Stack(
          WithDefaults(PropMap() + (MinimumWidth, 50) + (MinimumHeight, 50) + (Fill, "white") + (Stroke, "black")),
          Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
          Draggable.dragPart(drag, $posGraph, Label[PropMap], Center, MainHandle)
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

object Main {
  def main(args: Array[String]): Unit = {
    val action: M[Unit] = for {
      $model <- ReaderT.ask.map(_.$model)
      hover <- ReaderT.ask.map(_.hover)
      drag <- ReaderT.ask.map(_.drag)
      _ <- addChild(renderPosGraph($model, hover, drag))
      _ <- bindings.runForever
    } yield ()

    mountWithAction("app-container", LabeledGraph[PropMap](), action)
  }
}
