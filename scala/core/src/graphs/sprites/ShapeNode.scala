package semagrams.graphs

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._

import semagrams.util._
import semagrams._
import semagrams.rendering._
import semagrams.partprops._

import upickle.default._

enum ShapeOption derives ReadWriter:
  case RectShape, DiscShape
export ShapeOption._

enum ShapeProp[T: ReadWriter] extends Property {
  case Shape extends ShapeProp[ShapeOption]

  type Value = T
  val rw = summon[ReadWriter[T]]
}
export ShapeProp._

/** A sprite with shape determined by the `props` * */
case class ShapeNode(label: Property, init: PropMap) extends Sprite {
  def defaultProps = PropMap().set(Shape, RectShape)
  def requiredProps = Seq(Center)

  def setLabel: PropMap => PropMap = Sprite.setContent(label)

  def sprite(data: PropMap) =
    data.get(Shape) match
      case Some(RectShape) | None => Rect(label, init)
      case Some(DiscShape)        => Disc(label, init)

  def present(
      ent: PartTag,
      init: PropMap,
      updates: L.Signal[PropMap],
      eventWriter: L.Observer[state.Event]
  ) =
    val data = updates
      .map(init ++ _)
      .map(setLabel)

    val text = data.map(Sprite.innerText)

    val eltSig =
      data.splitOne(data => data.get(Shape))((shapeOpt, props, propSig) =>
        shapeOpt match
          case Some(RectShape) | None =>
            rect(Rect.geomUpdater(data), Rect.styleUpdater(data))
          case Some(DiscShape) =>
            circle(Disc.geomUpdater(data), Disc.styleUpdater(data))
      )

    g(
      cls := "shaperect",
      L.child <-- eltSig,
      L.child <-- text,
      state.MouseEvents.handlers(ent, eventWriter)
    )

  override def boundaryPt(
      data: PropMap,
      dir: Complex,
      subparts: Seq[PartTag] = Seq()
  ) =
    sprite(data).boundaryPt(data, dir, subparts)

  override def bbox(data: PropMap, subparts: Seq[PartTag]) =
    sprite(data).bbox(data, subparts)

  override def center(data: PropMap, subparts: Seq[PartTag]) =
    sprite(data).center(data, subparts)

  override def toTikz(p: PartTag, data: PropMap, visible: Boolean = true) =
    sprite(data).toTikz(p, data, visible)
}

object ShapeNode {

  val shapeProps = PropMap().set(Shape, RectShape)

  def apply() = new ShapeNode(Content, shapeProps)
  def apply(props: PropMap) = new ShapeNode(Content, shapeProps ++ props)
  def apply(label: Property) = new ShapeNode(label, shapeProps)
  def ShapeNode(label: Property, props: PropMap) =
    new ShapeNode(label, shapeProps ++ props)

}
