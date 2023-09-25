package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets._

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
case class ShapeNode(label:Property,val props: PropMap) extends Sprite {

  def setLabel: PropMap => PropMap = Sprite.setContent(label)
  
  def sprite(props:PropMap) = 
    props.get(Shape) match
    case Some(RectShape) | None => Rect(label,props)
    case Some(DiscShape) => Disc(label,props)
   




  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      eventWriter: L.Observer[Event]
  ) = 

    val data = updates
      .map(props ++ _.props)
      .map(setLabel)

    val text = Sprite.innerText(data)

    val eltSig = data.splitOne(props => props.get(Shape))(
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
      text,
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
    

  
    
  override def boundaryPt(_subent: Entity, data: ACSet, dir: Complex) =
    sprite(data.props).boundaryPt(_subent,data,dir)

  override def bbox(_subent: Entity, data: ACSet) = 
    sprite(data.props).bbox(_subent,data)

  override def center(_subent: Entity, data: ACSet) = 
    sprite(data.props).center(_subent,data)

  override def toTikz(p: Part, data: ACSet, visible: Boolean = true) =
    sprite(data.props).toTikz(p,data,visible)
}

object ShapeNode {

  val shapeProps = PropMap(Shape -> RectShape)

  def apply() = new ShapeNode(Content,shapeProps)
  def apply(props:PropMap) = new ShapeNode(Content,shapeProps ++ props)
  def apply(label:Property) = new ShapeNode(label,shapeProps)
  def ShapeNode(label:Property,props: PropMap) = new ShapeNode(label,shapeProps ++ props)

}
