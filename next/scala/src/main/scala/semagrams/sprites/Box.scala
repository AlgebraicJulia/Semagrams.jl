package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams.util.CustomAttr._
import semagrams.controllers._
import semagrams._

case class BoxData(
  dims: Complex,
  pos: Complex
)

class BoxDataAttr extends CustomSvgAttr[BoxData] {
  def applyAttrs(binder: SvgBinder[BoxData]): Unit = {
    binder(width, _.dims.x.toString)
    binder(height, _.dims.y.toString)
    binder(x, _.pos.x.toString)
    binder(y, _.pos.y.toString)
  }
}

val boxData = new BoxDataAttr()

enum BoxEvent {
  case NewParams(ent: Entity, params: BoxData)
}

enum Side {
  case Top
  case Bottom
  case Left
  case Right

  def sideBox(thickness: Double): BoxData => BoxData = {
    this match {
      case Top => { case BoxData(dims, pos) => BoxData(Complex(dims.x, thickness), Complex(pos.x, pos.y - thickness / 2)) }
      case Bottom => { case BoxData(dims, pos) => BoxData(Complex(dims.x, thickness), Complex(pos.x, pos.y + dims.y - thickness / 2)) }
      case Left => { case BoxData(dims, pos) => BoxData(Complex(thickness, dims.y), Complex(pos.x - thickness / 2, pos.y)) }
      case Right => { case BoxData(dims, pos) => BoxData(Complex(thickness, dims.y), Complex(pos.x + dims.x - thickness / 2, pos.y)) }
    }
  }

  def vertical: Boolean = {
    this match {
      case Top => true
      case Bottom => true
      case _ => false
    }
  }

  def sidePos: BoxData => Complex = {
    this match {
      case Top => { case BoxData(dims, pos) => Complex(pos.x, pos.y) }
      case Bottom => { case BoxData(dims, pos) => Complex(pos.x, pos.y + dims.y) }
      case Left => { case BoxData(dims, pos) => Complex(pos.x, pos.y) }
      case Right => { case BoxData(dims, pos) => Complex(pos.x + dims.x, pos.y) }
    }
  }

  def resize: (Complex, BoxData) => BoxData = {
    this match {
      case Top => { case (newpos, BoxData(dims, pos)) => BoxData(Complex(dims.x, (pos.y - newpos.y) + dims.y), Complex(pos.x, newpos.y)) }
      case Bottom => { case (newpos, BoxData(dims, pos)) => BoxData(Complex(dims.x, newpos.y - pos.y), Complex(pos.x, pos.y)) }
      case Left => { case (newpos, BoxData(dims, pos)) => BoxData(Complex((pos.x - newpos.x) + dims.x, dims.y), Complex(newpos.x, pos.y)) }
      case Right => { case (newpos, BoxData(dims, pos)) => BoxData(Complex(newpos.x - pos.x, dims.y), Complex(pos.x, pos.y)) }
    }
  }
}

// case class Box(
//   hover: HoverController,
//   drag: DragController,
//   updater: L.Observer[BoxEvent]
// ) extends Sprite {
//   type Model = BoxData

//   def makeSide(ent: Entity, model: L.Var[Model], side: Side) = {
//     val resize = side.resize
//     rect(
//       boxData <-- model.signal.map(side.sideBox(10)),
//       stroke := "none",
//       fill := "#fff",
//       fillOpacity := "0",
//       style := "cursor:"+(if side.vertical then "ns-resize" else "ew-resize"),
//       drag.draggable(model.signal.map(side.sidePos), updater.contramap((pos: Complex) => BoxEvent.NewParams(ent, resize(pos, model.now()))))
//     )
//   }

//   def present(ent: Entity, initModel: Model, updates: L.Signal[Model]) = {
//     val model = L.Var(initModel)
//     g(
//       rect(
//         updates --> model.writer,
//         boxData <-- updates,
//         stroke := "black",
//         fill <-- hover.switchState(ent, "lightgrey", "white"),
//         drag.draggable(updates.map(_.pos), updater.contramap(pos => BoxEvent.NewParams(ent, model.now().copy(pos = pos)))),
//         hover.hoverable(ent),
//       ),
//       makeSide(ent, model, Side.Top),
//       makeSide(ent, model, Side.Bottom),
//       makeSide(ent, model, Side.Left),
//       makeSide(ent, model, Side.Right),
//     )
//   }
// }
