package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.text._
import semagrams.util._

case class Disc() extends Sprite {
  def radius(data: PropMap): Double = {
    val textBox = boxSize(data(Content), data(FontSize))
    val center = data(Center)
    val innerSep = data(InnerSep)
    val d =
      data(MinimumWidth).max(textBox.x + innerSep).max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater(updates: L.Signal[PropMap]) = {
    List(
      cxy <-- updates.map(_(Center)),
      r <-- updates.map(radius(_).toString)
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
      updates: L.Signal[PropMap]
  ): RenderedSprite = {
    val box = circle(
      geomUpdater(updates),
      styleUpdater(updates)
    )
    val text = L.svg.text(
      xy <-- updates.map(_(Center)),
      L.svg.tspan(
        L.child <-- updates.map(p => L.textToNode(p(Content))),
        textAnchor := "middle",
        dominantBaseline := "central",
        style := "user-select: none"
      ),
      fontSize <-- updates.map(_(FontSize).toString)
    )

    val root = g(
      box,
      text
    )

    RenderedSprite(root, Map(MainHandle -> root))
  }

  def boundaryPt(data: PropMap, dir: Complex) = {
    val rad = radius(data)
    dir.normalize * rad + data(Center)
  }

  def boundaryNormal(data: PropMap, dir: Complex) = dir.normalize

}
