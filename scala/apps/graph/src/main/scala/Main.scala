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

type Boxes = LabeledGraph[Complex]

val addBox: Action[Boxes, Unit] = for {
  pos <- mousePos
  _ <- updateModelS(addLabeledVertex(pos))
} yield {}

// val remBox: Action[Boxes, Unit] = (for {
//   ent <- OptionT(hovered)
//   _ <- OptionT.liftF(updateModel(remVertex(ent.id)))
// } yield {}).value.map(_ => {})

val bindings = KeyBindings(
  Map(
    "a" -> addBox,
  )
)

type M[T] = Action[Boxes, T]
val L = actionLiftIO[Boxes]
val A = implicitly[Monad[[X] =>> Action[LabeledGraph[Complex],X]]]

val MyBox = WithDefaults(
  Box(),
  PropMap() + (MinimumWidth(), 50) + (MinimumHeight(), 50) + (Fill(), "white") + (Stroke(), "black")
)

extension(b: Boxes)
  def labeledVertices() = {
    val vs = b.vertices().toList
    vs.map(v => (v, b.subpart(Label[Complex](), v).get))
  }

def renderBoxes(
  $boxes: Var[LabeledGraph[Complex]],
  hover: HoverController,
  drag: DragController
) = {
  val renderedBoxes = $boxes.signal.map(_.labeledVertices()).split(_._1)(
    (v, init, $state) => {
      val hoverStream = hover.switchState(v, PropMap() + (Fill(), "lightgray"), PropMap())
      val rendered = MyBox.present(
        v, PropMap() + (Center(), init._2),
        $state.map(c => PropMap() + (Center(), c._2)).combineWith(hoverStream).map(_ ++ _)
      )
      val handle = rendered.handles(MainHandle())
      handle.amend(
        hover.hoverable(v),
        drag.draggable(
          $boxes.signal.map(_.subpart(Label[Complex](), v).get),
          Observer(pos => $boxes.update(g => g.setSubpart(Label[Complex](), v, pos)))
        )
      )
      rendered.root
    })

  svg.g(
    children <-- renderedBoxes
  )
}

object Main {
  def main(args: Array[String]): Unit = {
    val action: M[Unit] = for {
      $model <- ReaderT.ask.map(_.$model)
      hover <- ReaderT.ask.map(_.hover)
      drag <- ReaderT.ask.map(_.drag)
      _ <- addChild(renderBoxes($model, hover, drag))
      _ <- bindings.runForever
    } yield ()

    mountWithAction("app-container", LabeledGraph[Complex](), action)
  }
}
