package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets.abstr._

import upickle.default._
import com.raquo.laminar.nodes.ReactiveSvgElement

enum ShapeOption derives ReadWriter:
  case RectShape, DiscShape
export ShapeOption._

enum ShapeProp[T: ReadWriter] extends Property {
  case Shape extends ShapeProp[ShapeOption]
  
  type Value = T
  val rw = summon[ReadWriter[T]]
}
export ShapeProp._



/** A sprite with shape determined by the `props` **/
case class ShapeNode[D:PartData](label:Property,init: D) extends Sprite[D] {
  def defaultProps = PropMap().set(Shape,RectShape)
  def requiredProps = Seq(Center)

  def setLabel: D => D = Sprite.setContent(label)
  
  def sprite(data:D) = 
    data.tryProp(Shape) match
    case Some(RectShape) | None => Rect(label,init)
    case Some(DiscShape) => Disc(label,init)
   




  def present(
      ent: Part,
      init: D,
      updates: L.Signal[D],
      eventWriter: L.Observer[Event]
  ) = 
    val data = updates
      .map(init.merge(_))
      .map(setLabel)

    val text = data.map(Sprite.innerText)

    val eltSig = data.splitOne(data => data.tryProp(Shape))(
      (shapeOpt,props,propSig) => shapeOpt match
        case Some(RectShape) | None =>
          rect(Rect.geomUpdater(data),Rect.styleUpdater(data))
        case Some(DiscShape) =>
          circle(Disc.geomUpdater(data),Disc.styleUpdater(data))
    )

    // def geomUpdater(data: L.Signal[PropMap]) = //: L.Signal[Seq[L.Modifier[L.SvgElement[]]]] =
    //   data.map(props => props.get(Shape) match
    //     case Some(RectShape) | None =>
    //       Rect.geomUpdater(data)
    //     case Some(DiscShape) =>
    //       Disc.geomUpdater(data)  
    //   )

    g(
      cls := "shaperect",
      L.child <-- eltSig,
      L.child <-- text,
      MouseEvents.handlers(ent,eventWriter),
      
      // rect(
      //   stroke := "black",
      //   text,
      //   MouseEvents.handlers(ent,eventWriter),
      //   Rect.geomUpdater(data),
      //   Rect.styleUpdater(data)
      // )
    )

    // g(
    //   child <-- data.map(_.get(Shape) match
    //     case Some(RectShape) | None => (Rect,rect)
    //     case Some(DiscShape) => (Disc,circle)
    //   ).map((semaShape,svgShape) => svgShape(
    //     semaShape.geomUpdater,
    //     semaShape.styleUpdater        
    //   ))
    // )


    // val elt = g(
    //   cls := "shapenode",
    //   L.child <-- data.map{
    //     sprite(_)
    //       .present(ent,init,updates,eventWriter)      
    //   },
    // )
    // elt
    

  
    
  override def boundaryPt(data: D, dir: Complex, subparts:Seq[Part] = Seq()) =
    sprite(data).boundaryPt(data,dir,subparts)

  override def bbox(data: D, subparts:Seq[Part]) = 
    sprite(data).bbox(data,subparts)

  override def center(data: D, subparts:Seq[Part]) = 
    sprite(data).center(data,subparts)

  override def toTikz(p: Part, data: D, visible: Boolean = true) =
    sprite(data).toTikz(p,data,visible)
}

object ShapeNode {

  val shapeProps = PropMap(Shape -> RectShape)

  def apply() = new ShapeNode(Content,shapeProps)
  def apply(props:PropMap) = new ShapeNode(Content,shapeProps ++ props)
  def apply(label:Property) = new ShapeNode(label,shapeProps)
  def ShapeNode(label:Property,props: PropMap) = new ShapeNode(label,shapeProps ++ props)

}
