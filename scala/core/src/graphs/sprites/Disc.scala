package semagrams.graphs

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._

import semagrams._
import semagrams.util._
import semagrams.rendering._
import semagrams.partprops._

/** A Sprite for a geometric Disc shape.
  *
  * Auto-resizes based on the content inside.
  */
case class Disc(label: Property, init: PropMap) extends Sprite {
  import Disc._

  def requiredProps = Seq(Center)
  def defaultProps = Sprite.defaultProps

  def setLabel: PropMap => PropMap = Sprite.setContent(label)

  def present(
      ent: PartTag,
      init: PropMap,
      updates: L.Signal[PropMap],
      eventWriter: L.Observer[state.Event]
  ): L.SvgElement = {
    val data = updates
      .map(init.softSetProps(defaultProps) ++ _)
      .map(setLabel)

    val box = circle(
      geomUpdater(data),
      styleUpdater(data)
    )

    val text = data.map(Sprite.innerText)

    val bg = image(
      cls := "disc-bg",
      href <-- data.map(_(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
      Rect.geomUpdater(data),
      /* Prevent selection outside semagram window (on double click) */
      L.svg.style := "user-select:none"
    )

    g(
      box,
      bg,
      L.child <-- text,
      state.MouseEvents.handlers(ent, eventWriter)
    )
  }

  override def boundaryPt(
      init: PropMap,
      dir: Complex,
      subparts: Seq[PartTag] = Seq()
  ) = {
    val data = init.softSetProps(defaultProps)
    val rad = radius(data) + data
      .get(OuterSep)
      .getOrElse(defaultProps(OuterSep))

    data.get(Center).map(dir.normalize * rad + _)
  }

  override def bbox(data: PropMap, subparts: Seq[PartTag] = Seq()) =
    center(data, subparts).map(
      rendering.BoundingBox(_, Complex(2 * radius(init), 2 * radius(init)))
    )

  override def center(data: PropMap, subparts: Seq[PartTag] = Seq()) =
    data.get(Center)

  override def toTikz(p: PartTag, data: PropMap, visible: Boolean = true) =
    tikzNode(
      "circle",
      p.keyPart.tikzName,
      data.get(Center).getOrElse(Complex(0, 0)),
      data
        .get(label)
        .getOrElse("")
        .toString
        .flatMap(_ match
          case '\n' => "\\\\"
          case ch   => ch.toString()
        ),
      visible
    )

}

object Disc {

  import Sprite.defaultProps

  def radius(data: PropMap): Double = {
    val textBox =
      boxSize(data.get(Content), data.get(FontSize), split = true)
    val innerSep = data.get(InnerSep).getOrElse(defaultProps(InnerSep))
    val d = data
      .get(MinimumWidth)
      .getOrElse(defaultProps(MinimumWidth))
      .max(textBox.x + innerSep)
      .max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater(data: L.Signal[PropMap]) = {
    List(
      cxy <-- data.map(_.get(Center).getOrElse(defaultProps(Center))),
      r <-- data.map(radius(_).toString)
    )
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(_(Fill).toString),
      opacity <-- data.map(d =>
        if d.get(Highlight).isDefined then ".8" else "1"
      ),
      stroke <-- data.map(
        _.get(Stroke).getOrElse(defaultProps(Stroke)).toString
      ),
      style <-- data.map(_.get(Style).getOrElse(defaultProps(Style)))
    )
  }

  def apply() = new Disc(Content, defaultProps)
  def apply(props: PropMap) = new Disc(Content, defaultProps ++ props)
  def apply(label: Property) = new Disc(label, defaultProps)
  def apply(label: Property, props: PropMap) =
    new Disc(label, defaultProps ++ props)

  def boundaryNormal(data: PropMap, dir: Complex) = dir.normalize

}
