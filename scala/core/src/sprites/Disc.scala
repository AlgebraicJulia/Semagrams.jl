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
case class Disc(props: PropMap) extends Sprite {
  def radius(data: PropMap): Double = {
    val textBox = boxSize(data(Content), data(FontSize))
    val innerSep = data.get(InnerSep).getOrElse(0.0)
    val d = data.get(MinimumWidth).getOrElse(0.0)
      .max(textBox.x + innerSep).max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater(data: L.Signal[PropMap]) = {
    List(
      cxy <-- data.map(_.get(Center).getOrElse(Complex(100,100))),
      r <-- data.map(radius(_).toString)
    )
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(d =>
        if d.get(Hovered).isDefined then "lightgrey" else d(Fill)
      ),
      stroke <-- data.map(_(Stroke)),
      style <-- data.map(_.get(Style).getOrElse(""))
    )
  }

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(props ++ _.props)
    val box = circle(
      geomUpdater(data),
      styleUpdater(data)
    )
    val text = L.svg.text(
      xy <-- data.map(_.get(Center).getOrElse(Complex(100,100))),
      L.svg.tspan(
        L.child <-- data.map(p => L.textToNode(p(Content))),
        textAnchor := "middle",
        dominantBaseline := "central",
        style := "user-select: none"
      ),
      fontSize <-- data.map(_(FontSize).toString)
    )

    val bg = image(
      href <-- data.map(_(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
      Rect.geomUpdater(data)
    )

    val root = g(
      box,
      bg,
      text
    )

    attachHandlers(ent, root)

    root
  }

  override def boundaryPt(subent: Entity, orig: ACSet, dir: Complex) = {
    val data = props ++ orig.props
    val rad = radius(data) + data(OuterSep)
    Some(dir.normalize * rad + data.get(Center).getOrElse(Complex(100,100)))
  }

  override def bbox(subent: Entity, data: ACSet) = 
    center(subent,data).map(
      BoundingBox(_,Complex(2*radius(props),2*radius(props)))
    )

  override def center(_subent: Entity, data: ACSet) = 
    data.props.get(Center)

}

object Disc {
  val defaults = PropMap()
    + (Content, "")
    + (ImageURL, "")
    + (FontSize, 14)
    + (Fill, "white")
    + (Stroke, "black")
    + (InnerSep, 10)
    + (OuterSep, 5)
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)

  def apply() = new Disc(defaults)

  def apply(pm: PropMap) = new Disc(defaults ++ pm)

  def boundaryNormal(data: PropMap, dir: Complex) = dir.normalize

}
