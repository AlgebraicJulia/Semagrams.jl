package graph

import semagrams.*
import semagrams.util.*
import semagrams.acsets.{*, given}
import semagrams.actions.*
import cats.data._
import cats.Monad
import cats.effect.IO
import cats.data.OptionT
import semagrams.sprites._
import com.raquo.laminar.api.L.{*, given}
import semagrams.controllers._

/**
 * TODO:
 * - Clean up creation of boxData from boxSpriteMaker and $posGraph
 * - Clean up use of OptionT[Action[PosGraph,_],A]
 * - Clean up use of boxData in edges
 * - Recursive deletion
 * - Prettier edges
 * - Dialogue for edge creation
 *
 *
 * What do I need?
 *
 * Basically, I want something like split, but it keeps track of a map of things
 * rather than a list. I feel like this should be possible using a Var containing
 * a map, and then an updater for that Var that takes in a new list of things,
 * and then updates the map based on new entities.
 *
 * The values in the map should be the rendered sprite, and then also a signal
 * of the propmap used for that rendered sprite, along with the initial values
 * of that propmap.
 *
 * This signal of PropMaps for each entity can then be used in later entity types,
 * using Signal.combine.
 */

/**
 * A positioned graph
 */
type PosGraph = LabeledGraph[Complex]

val addBox: Action[PosGraph, Unit] = for {
  pos <- mousePos
  _ <- updateModelS(addLabeledVertex(pos))
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
  $posGraph: Var[LabeledGraph[Complex]],
  hover: HoverController,
  drag: DragController
) = {
  val boxMap = SpriteMap[PosGraph, Elt[V.type]](
    $posGraph.signal,
    Box(),
    s => {
      s.parts(V).toList.map(v => (v, PropMap() + (Center, s.subpart(Label[Complex], v).get)))
    },
    Stack(
      WithDefaults(PropMap() + (MinimumWidth, 50) + (MinimumHeight, 50) + (Fill, "white") + (Stroke, "black")),
      Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
      Draggable.dragPart(drag, $posGraph, Label[Complex], MainHandle)
    ),
  )

  val arrowMap = SpriteMap[(PosGraph, Map[Elt[V.type], PropMap]), Elt[E.type]](
    Signal.combine($posGraph.signal, boxMap.$propMaps),
    Arrow(),
    { case (s, boxes) => {
       s.parts(E).toList.map(
         e => {
           val src = s.subpart(Src, e).get
           val tgt = s.subpart(Tgt, e).get
           (e, PropMap() + (Start, boxes(src)(Center)) + (End, boxes(tgt)(Center)))
         }
       )}},
    WithDefaults(PropMap() + (Stroke, "black"))
  )

  svg.g(
    boxMap.attach,
    arrowMap.attach
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

    mountWithAction("app-container", LabeledGraph[Complex](), action)
  }
}
