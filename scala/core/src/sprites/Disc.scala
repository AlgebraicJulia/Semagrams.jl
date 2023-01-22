package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.util._

case class Disc(props: PropMap) extends Sprite {
  def radius(data: PropMap): Double = {
    val textBox = boxSize(data(Content), data(FontSize))
    val center = data(Center)
    val innerSep = data(InnerSep)
    val d =
      data(MinimumWidth).max(textBox.x + innerSep).max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater(data: L.Signal[PropMap]) = {
    List(
      cxy <-- data.map(_(Center)),
      r <-- data.map(radius(_).toString)
    )
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(_(Fill)),
      stroke <-- data.map(_(Stroke)),
      style <-- data.map(_.get(Style).getOrElse(""))
    )
  }

  def present(
      ent: Entity,
      init: PropMap,
      updates: L.Signal[PropMap],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(props ++ _)
    val box = circle(
      geomUpdater(data),
      styleUpdater(data)
    )
    val text = L.svg.text(
      xy <-- data.map(_(Center)),
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
      Rect.geomUpdater(data),
    )

    val root = g(
      box,
      bg,
      text
    )

    attachHandlers(ent, root)

    root
  }

  override def boundaryPt(subent: Entity, orig: PropMap, dir: Complex) = {
    val data = props ++ orig
    val rad = radius(data) + data(OuterSep)
    Some(dir.normalize * rad + data(Center))
  }

  override def bbox(subent: Entity, data: PropMap) = Rect(props).bbox(subent, data)
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
}
