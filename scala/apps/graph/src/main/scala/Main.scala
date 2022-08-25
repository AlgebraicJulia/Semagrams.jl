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
  val boxSpriteMaker = SpriteMaker[PosGraph](
    Box(),
    Stack(
      WithDefaults(PropMap() + (MinimumWidth, 50) + (MinimumHeight, 50) + (Fill, "white") + (Stroke, "black")),
      Hoverable(hover, MainHandle, PropMap() + (Fill, "lightgray")),
      Draggable.dragPart(drag, $posGraph, Label[Complex], MainHandle)
    ),
    (s, ent) => PropMap() + (Center, s.subpart(Label[Complex], ent.asInstanceOf[Elt[V.type]]).get)
  )

  val edgeSprite = Arrow()

  val $boxData = $posGraph.signal.map(
    posGraph => posGraph.parts(V).toList.map(v => (v, boxSpriteMaker.extractor(posGraph, v)))
  ).split(_._1)(
    { case (ent, (_, p), updates) => {
       val q = boxSpriteMaker.middleware.updateProps(ent, p)
       val qs = boxSpriteMaker.middleware.updatePropsS(ent, updates.map(_._2))
       val rs = boxSpriteMaker.middleware.modifyRendered(ent, boxSpriteMaker.sprite.present(ent, p, qs))
       (ent, (qs, rs))
     }
    }
  )

  val edges = Signal.combine($boxData.map(_.toMap), $posGraph.signal).map(
    (boxData, posGraph) => {
      posGraph.parts(E).toList.map(
        e => {
          val s = posGraph.subpart(Src, e).get
          val t = posGraph.subpart(Tgt, e).get
          val (sps,_) = boxData(s)
          val (tps,_) = boxData(t)
          (e, Signal.combine(sps, tps).map({ case (sp, tp) => PropMap() + (Stroke, "black") + (Start, sp(Center)) + (End, tp(Center)) }))
        }
      )
    }
  ).split(_._1)(
    { case (ent, (_, ps), _) => {
       edgeSprite.present(ent, PropMap(), ps).root
     }
    }
  )

  svg.g(
    children <-- edges,
    children <-- $boxData.map(_.map({ case (_, (_, rs)) => rs.root }))
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
