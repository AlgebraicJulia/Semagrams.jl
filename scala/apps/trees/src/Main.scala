
package semagrams.trees

import cats.Monad
import cats.data.OptionT
import cats.data._
import cats.effect.IO
import cats.effect.syntax.all._
import com.raquo.laminar.api.L.{_, given}
import org.scalajs.dom
import semagrams._
import semagrams.acsets.{_, given}
import semagrams.actions._
import semagrams.controllers._

import monocle._
import monocle.macros.GenIso

import semagrams.sprites._
import semagrams.text._
import semagrams.util._
import semagrams.widgets._
import upickle.default._

import scala.collection.MapView.Keys
import scala.scalajs.js.annotation.JSExportTopLevel

import Petris._
import org.scalajs.dom.KeyboardEvent
import com.raquo.laminar.nodes.ReactiveHtmlElement

import scalacss.DevDefaults.StyleA
import scalacss.internal.mutable.GlobalRegistry

import com.raquo.laminar.api.L

val CssSettings = scalacss.devOrProdDefaults
import CssSettings._


// Schema elements

case object Node extends Ob
case object Edge extends Ob

case object src extends HomWithDom {
  val dom = Edge
  val codom = Node
}
case object tgt extends HomWithDom {
  val dom = Edge
  val codom = Node
}

case object label extends AttrWithDom with PValue[String] {
  val dom = Node
}

// Schema/Instance types

case object GraphSchema extends StaticSchema {
  val schema = BasicSchema(Node,Edge,src,tgt,name)
}
type GraphSchemaType = GraphSchema.type

type Graph = ACSet[GraphSchemaType]

object Graph {
  def apply() = ACSet[TreeSchemaType]()
}


// Interaction logic

val ops = Action.ops[Graph]
val aops = summon[ACSetOps[GraphSchemaType]]

val addNode = mousePos.flatMap(p =>
  updateModelS()
)

val addNode = addPartPos[GraphSchemaType](
  Node,
  PropMap().set(label,"")
)

def dragParent[S: IsSchema](
    ob: Ob,
    src: Hom,
    tgt: Hom,
    s: Part
): Action[ACSet[S], Unit] = {
  val aops = summon[ACSetOps[S]]
  for {
    drag <- ops[ACSet[S]].ask.map(_.drag)
    $model <- ops[ACSet[S]].ask.map(_.$model)
    p <- mousePos
    e <- updateModelS(aops.addPart(ob, PropMap().set(src, s).set(End, p)))
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setSubpart(End, e, p))))
      t <- fromMaybe(hoveredPart[ACSet[S]](tgt.codom))
      _ <- updateModelS(aops.setSubpart(tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- ops.delay(drag.$state.set(None))
      _ <- updateModelS(aops.remPart(e))
    } yield ())
    _ <- update
  } yield ()
}




val zoomFactor = 1.1
val bindings = Bindings[Tree, Unit](
  keyDown("+").andThen(zoomAtMouse(zoomFactor)),
  keyDown("-").andThen(zoomAtMouse(1/zoomFactor)),
  clickOn(ClickType.Single,MouseButton.Left,BackgroundType).andThen(dragPan),
  keyDown("n").andThen(
    addNode.flatMap(_ => update)
  ),
  keyDown("d").andThen(remPart),
  clickOn(ClickType.Single,MouseButton.Left,Node)
    .withMods(KeyModifier.Shift)
    .flatMap(s => dragEdge(Node,Id(Node),parent,s.asInstanceOf[Part])),
  clickOn(ClickType.Single,MouseButton.Right,Node)
    .flatMap(n => showPopoverUntil(Seq("Hi There"), keyDown("Escape"))),
  keyDown("Escape").andThen(showPopoverUntil(Seq("Esc test"),keyDown("Escape"))),
  clickOn(ClickType.Single,MouseButton.Left,Node)
    .flatMap(n => dragPart(n.asInstanceOf[Part])),
  clickOn(ClickType.Single, MouseButton.Left, Node)
    .andThen(alert("Hi!"))
)


// def dragParent[S: IsSchema](
//     ob: Ob,
//     src: Hom,
//     tgt: Hom,
//     s: Part
// ): Action[ACSet[S], Unit] = {
//   val aops = summon[ACSetOps[S]]
//   for {
//     drag <- ops.ask.map(_.drag)
//     $model <- ops.ask.map(_.$model)
//     p <- mousePos
//     e <- updateModelS(aops.addPart(ob, PropMap().set(src, s).set(End, p)))
//     _ <- (for {
//       _ <- drag.drag(Observer(p => $model.update(_.setSubpart(End, e, p))))
//       t <- fromMaybe(hoveredPart[ACSet[S]](tgt.codom))
//       _ <- updateModelS(aops.setSubpart(tgt, e, t))
//     } yield ()).onCancelOrError(for {
//       _ <- ops.delay(drag.$state.set(None))
//       _ <- updateModelS(aops.remPart(e))
//     } yield ())
//     _ <- update
//   } yield ()
// }



def renderTrees(
  $trees: Var[Tree],
  hover: HoverController,
  mouse: MouseController
) = {
  val spriteMaps = SpriteMaps[Tree](
    $trees.signal,
    List(
      SpriteMaker[Tree](
        Disc(),
        (t, _) =>
          t.parts(Node)
            .toList
            .map(n =>
              (
                n,
                t.props(n)
                  + (Content, t.trySubpart(name,n).
                  getOrElse(""))
              )  
            ),
        Stack(
          WithDefaults(
            PropMap()
              + (MinimumWidth, 40)
              + (MinimumHeight, 40)
              + (Fill, "#6C9AC3")
              + (Stroke, "black")
              + (InnerSep, 7)
              + (FontSize, 16)
          ),
          Hoverable(
            hover,
            MainHandle,
            PropMap() + (Style, "filter: opacity(0.7)")
          ),
          Clickable(mouse, MainHandle)
        )

      )
    )
  )

  svg.g(
    spriteMaps.attach
  )
}

val serializer = ACSet.rw[TreeSchemaType]


// CSS

object PS extends StyleSheet.Inline {
  import dsl._

  val flex1 = mixin(
    display.flex,
    flex := "1"
  )

  val row = style(
    flex1,
    flexDirection.row
  )

  val col = style(
    flex1,
    flexDirection.column
  )
}

val PSrendered = PS.render[String]


// Main

object Main {

  @JSExportTopLevel("main")
  def main(el: dom.Element): Unit = {
    val action = for {
      $model <- ops.ask.map(_.$model)
      hover <- ops.ask.map(_.hover)
      mouse <- ops.ask.map(_.mouse)
      _ <- addRelative(renderTrees($model,hover,mouse))
      _ <- bindings.runForever
    } yield ()

    dom.document.querySelector("head").appendChild(styleTag(PSrendered).ref)

    plutoMain(el,Tree(),serializer,action)
  }
}




























