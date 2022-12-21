package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.text._
import semagrams.util._

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))

object Box {
  def geom(data: PropMap): (Complex, Complex) = {
    val textBox = boxSize(data(Content), data(FontSize))
    val center = data(Center)
    val innerSep = data(InnerSep)
    val width = data(MinimumWidth).max(textBox.x + innerSep)
    val height = data(MinimumHeight).max(textBox.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater(updates: L.Signal[PropMap]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(_(Fill)),
      stroke <-- data.map(_(Stroke)),
      style <-- data.map(_.get(Style).getOrElse(""))
    )
  }
}

case class Disc(defaults: PropMap) extends Sprite {
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

  def render(
      ent: Entity,
      init: PropMap,
      updates: L.Signal[PropMap]
  ): RenderedSprite = {
    val data = updates.map(defaults ++ _)
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
      Box.geomUpdater(data),
    )

    val root = g(
      box,
      bg,
      text
    )

    RenderedSprite(root, Map(MainHandle -> root))
  }

  def boundaryPt(orig: PropMap, dir: Complex) = {
    val data = defaults ++ orig
    val rad = radius(data) + data(OuterSep)
    dir.normalize * rad + data(Center)
  }
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
}
