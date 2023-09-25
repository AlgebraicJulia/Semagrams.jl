package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.util._
import semagrams.acsets._

/** A Sprite for a geometric Disc shape.
  *
  * Auto-resizes based on the content inside.
  */
case class Disc(label:Property,props: PropMap) extends Sprite {
  import Disc._

  def setLabel: PropMap => PropMap = Sprite.setContent(label)



  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {
    val data = updates
      .map(props ++ _.props)
      .map(setLabel)
    
    val box = circle(
      geomUpdater(data),
      styleUpdater(data)
    )

    val text = Sprite.innerText(data) 
      
    //   L.svg.text(
    //   xy <-- data.map(_.get(Center).getOrElse(Complex(100, 100))),
    //   L.svg.tspan(
    //     L.child <-- data.map(p => L.textToTextNode(p(Content))),
    //     textAnchor := "middle",
    //     dominantBaseline := "central",
    //     style := "user-select: none"
    //   ),
    //   fontSize <-- data.map(_(FontSize).toString)
    // )

    val bg = image(
      cls := "disc-bg",
      href <-- data.map(_(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
      Rect.geomUpdater(data)
    )

    g(
      box,
      bg,
      text,
      MouseEvents.handlers(ent, eventWriter)
    )
  }

  override def boundaryPt(subent: Entity, orig: ACSet, dir: Complex) = {
    val data = props ++ orig.props
    val rad = radius(data) + data.get(OuterSep)
      .getOrElse(Disc.defaults(OuterSep))
    
    data.get(Center).map(dir.normalize * rad + _)
  }

  override def bbox(subent: Entity, data: ACSet) =
    center(subent, data).map(
      BoundingBox(_, Complex(2 * radius(props), 2 * radius(props)))
    )

  override def center(_subent: Entity, data: ACSet) =
    data.props.get(Center)

  override def toTikz(p: Part, data: ACSet, visible: Boolean = true) = tikzNode(
    "circle",
    p.tikzName,
    data.props.get(Center).getOrElse(Complex(0, 0)),
    data.props.get(label).getOrElse("").toString
      .flatMap(_ match
        case '\n' => "\\\\"
        case ch   => ch.toString()
      ),
    visible
  )

}

object Disc {

  def radius(data: PropMap): Double = {
    val textBox = boxSize(data.get(Content), data.get(FontSize),split = true)
    val innerSep = data.get(InnerSep).getOrElse(defaults(InnerSep))
    val d = data
      .get(MinimumWidth)
      .getOrElse(defaults(MinimumWidth))
      .max(textBox.x + innerSep)
      .max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater(data: L.Signal[PropMap]) = {
    List(
      cxy <-- data.map(_.get(Center).getOrElse(defaults(Center))),
      r <-- data.map(radius(_).toString)
    )
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(d =>
        if d.get(Hovered).isDefined then "lightgrey" else d.get(Fill).getOrElse("white")
      ),
      stroke <-- data.map(_.get(Stroke).getOrElse(defaults(Stroke))),
      style <-- data.map(_.get(Style).getOrElse(defaults(Style)))
    )
  }




  val defaults = PropMap()
    + (Center, Complex(100,100))
    + (Content, "")
    + (ImageURL, "")
    + (FontSize, 14)
    + (Fill, "white")
    + (Stroke, "black")
    + (InnerSep, 10)
    + (OuterSep, 5)
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)
    + (Style,"")

  def apply() = new Disc(Content,defaults)
  def apply(props:PropMap) = new Disc(Content,defaults ++ props)
  def apply(label:Property) = new Disc(label,defaults)
  def apply(label:Property,props: PropMap) = new Disc(label,defaults ++ props)

  def boundaryNormal(data: PropMap, dir: Complex) = dir.normalize

}
