package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.Sprite
import semagrams.Sprite._
import semagrams.util._
import semagrams.util.CustomAttr._
import com.raquo.laminar.nodes.ReactiveElement
import com.raquo.domtypes.jsdom.defs.events.TypedTargetMouseEvent
import org.scalajs.dom
import semagrams._
import semagrams.controllers._

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

def draggable[El<:L.SvgElement](
  ent: Entity,
  center: L.Source[Complex],
  bus: L.Observer[EditorEvent]) = {
  val curCenter = L.Var(Complex(0,0))
  CustomModifier[El](el =>
    el.amend(
      center --> curCenter.writer,
      L.onMouseDown.map(
        ev => {
          val c = curCenter.now()
          val init = Complex(ev.clientX, ev.clientY)
          val offset = c - init
          DragStartEvent(pos => MoveEntityEvent(ent, pos + offset))
        }) --> bus,
    )
  )
}

def hoverable[El<:L.SvgElement](ent: Entity, bus: L.Observer[EditorEvent]) = {
  CustomModifier[El](el =>
    el.amend(
      L.onMouseEnter.map(_ => HoverStartEvent(ent)) --> bus,
      L.onMouseLeave.map(_ => HoverEndEvent(ent)) --> bus,
    )
  )
}

enum BoxEvent {
  case Move(pos: Complex)
}

case class Box(hoverHandle: ControllerHandle[Option[Entity]]) extends Sprite {
  type Model = BoxData

  def present(
    ent: Entity,
    initModel: BoxData,
    updates: L.Signal[BoxData],
    $reg: L.Signal[ControllerRegistry],
    bus: L.Observer[EditorEvent]
  ) = rect(
    boxData <-- updates,
    stroke := "black",
    fill <-- $reg.map(
      reg => if (reg.state(hoverHandle) == Some(ent)) {
        "lightgrey"
      } else {
        "white"
      }
    ),
    draggable(ent, updates.map(_.pos), bus),
    hoverable(ent, bus)
  )

}
