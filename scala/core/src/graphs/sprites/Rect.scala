package semagrams.graphs

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams.rendering._
import semagrams._
import semagrams.state._
import semagrams.partprops._

/** A sprite for geometric rectangles
  *
  * Resizes automatically corresponding to its content.
  */
case class Rect(label: Property, val rectInit: PropMap) extends Sprite:
  import Rect._

  def defaultProps = Sprite.defaultProps
  def requiredProps = Seq(Center)

  def setLabel: PropMap => PropMap = Sprite.setContent(label)
  def present(
      ent: PartTag,
      init: PropMap,
      updates: L.Signal[PropMap],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {

    val initData = rectInit ++ init
    val data = updates
      .map(initData ++ _)
      .map(setLabel)

    val text = data.map(Sprite.innerText)

    val box = rect(
      cls := "rect-box",
      geomUpdater(data),
      styleUpdater(data)
    )

    val bg = image(
      cls := "rect-bg",
      Rect.geomUpdater(data),
      Rect.bgUpdater(data)
    )

    val root = g(
      cls := "rect-root",
      box,
      L.child <-- text,
      bg,
      MouseEvents.handlers(ent, eventWriter),
      /* Prevent selection outside semagram window (on double click) */
      L.svg.style := "user-select:none"
    )

    root
  }

  override def boundaryPt(
      data: PropMap,
      dir: Complex,
      subparts: Seq[PartTag] = Seq()
  ) = {
    /* Normalize to first quadrant */
    val pm = rectInit ++ data
    val (_, boxdims) = geom(pm)
    val os = pm(OuterSep)
    val dims = Complex(boxdims.x + 2 * os, boxdims.y + 2 * os)
    val q1dir = Complex(dir.x.abs, dir.y.abs)
    val q1pt = if (q1dir.x == 0) {
      Complex(0, dims.y / 2)
    } else if (q1dir.y == 0) {
      Complex(dims.x / 2, 0)
    } else if (q1dir.x * dims.y > q1dir.y * dims.x) {
      Complex(
        dims.x / 2,
        (q1dir.y * dims.x) / (q1dir.x * 2)
      )
    } else {
      Complex(
        (q1dir.x * dims.y) / (q1dir.y * 2),
        dims.y / 2
      )
    }
    Some(
      Complex(q1pt.x * dir.x.sign, q1pt.y * dir.y.sign) + data
        .get(Center)
        .getOrElse(Complex(100, 100))
    )
  }

  override def bbox(data: PropMap, subparts: Seq[PartTag] = Seq()) = {
    val (pos, dims) = geom(rectInit ++ data)
    Some(BoundingBox(pos, dims))
  }

  override def center(data: PropMap, subparts: Seq[PartTag] = Seq()) =
    data.get(Center)

  override def toTikz(p: PartTag, data: PropMap, visible: Boolean = true) =
    tikzNode(
      "rectangle",
      p.keyPart.tikzName,
      data.get(Center).getOrElse(Complex(0, 0)),
      data(Content)
        .flatMap(_ match
          case '\n' => "\\\\"
          case ch   => ch.toString()
        ),
      visible
    )

object Rect:
  import Sprite.defaultProps

  def geom(data: PropMap): (Complex, Complex) = {
    val props = data.softSetProps(defaultProps)
    val textRect = boxSize(
      props(Content),
      props(FontSize),
      split = true
    )
    val center = props
      .get(Center)
      .getOrElse(throw msgError(s"missing center in $data"))
    val innerSep = props(InnerSep)
    val width = props(MinimumWidth)
      .max(textRect.x + innerSep)
    val height = props(MinimumHeight)
      .max(textRect.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater(updates: L.Signal[PropMap]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater(updates: L.Signal[PropMap]) = {
    val props = updates.map(_.softSetProps(defaultProps))

    List(
      fill <-- props.map(d =>
        if d.contains(Highlight)
        then d(Fill).lighten(.5).toString
        else d(Fill).toString
      ),
      strokeWidth <-- props.map(d =>
        if d.contains(Selected)
        then "3"
        else "1"
      ),
      stroke <-- props.map(data =>
        data
          .get(Stroke)
          .getOrElse(
            RGB("black")
          )
          .toString
      ),
      style <-- props.map(_(Style))
    )
  }
  def bgUpdater(data: L.Signal[PropMap]) = {
    val props = data.map(_.softSetProps(defaultProps))
    List(
      cls := "disc-bg",
      href <-- props.map(_(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none"
    )
  }

  def apply() = new Rect(Content, defaultProps)
  def apply(data: PropMap) =
    new Rect(Content, data.softSetProps(defaultProps))
  def apply(label: Property) =
    new Rect(label, defaultProps)
  def apply(label: Property, data: PropMap) =
    new Rect(label, data.softSetProps(defaultProps))
