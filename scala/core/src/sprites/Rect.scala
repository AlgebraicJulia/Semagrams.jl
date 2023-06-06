package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets._

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))

/** A sprite for geometric rectangles
  *
  * Resizes automatically corresponding to its content.
  */
case class Rect(val props: PropMap) extends Sprite {
  import Rect._

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val data = updates.map(props ++ _.props)

    val text = L.svg.text(
      xy <-- data.map(pm =>
        pm.get(Center)
          .getOrElse(throw msgError(s"propmap $pm missing `Center`"))
      ),
      L.children <-- data.map(p =>
        val splits = p(Content).split('\n').zipWithIndex
        val l = splits.length
        splits.toIndexedSeq.map({ case (t, i) =>
          L.svg.tspan(
            L.textToTextNode(t),
            textAnchor := "middle",
            x <-- data.map(p =>
              p.get(Center).getOrElse(Complex(50, 50)).x.toString()
            ),
            y <-- data.map(p =>
              (p.get(Center).getOrElse(Complex(100, 100)).y + p(
                FontSize
              ) * (i + 1 - l / 2.0)).toString()
            ),
            style := "user-select: none"
          )
        })
      ),
      fontSize <-- data.map(_(FontSize).toString),
      pointerEvents := "none"
    )

    val box = rect(
      geomUpdater(data),
      styleUpdater(data)
    )

    val root = g(
      box,
      text
    )

    attachHandlers(ent, root)

    root
  }

  override def boundaryPt(_subent: Entity, data: ACSet, dir: Complex) = {
    // Normalize to first quadrant
    val pm = props ++ data.props
    val (_, boxdims) = geom(pm)
    val os = (pm)(OuterSep)
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
      Complex(q1pt.x * dir.x.sign, q1pt.y * dir.y.sign) + data.props
        .get(Center)
        .getOrElse(Complex(100, 100))
    )
  }

  override def bbox(_subent: Entity, data: ACSet) = {
    val (pos, dims) = geom(props ++ data.props)
    Some(BoundingBox(pos, dims))
  }

  override def center(_subent: Entity, data: ACSet) = Some(
    data.props.get(Center).getOrElse(Complex(100, 100))
  )

  override def toTikz(p: Part, data: ACSet, visible: Boolean = true) = tikzNode(
    "rectangle",
    p.tikzName,
    data.props.get(Center).getOrElse(Complex(0, 0)),
    data.props
      .get(Content)
      .getOrElse("")
      .flatMap(_ match
        case '\n' => "\\\\"
        case ch   => ch.toString()
      ),
    visible
  )

}

object Rect {
  def geom(data: PropMap): (Complex, Complex) = {
    val textRect = boxSize(data(Content), data(FontSize))
    val center = data.get(Center).getOrElse(Complex(100, 100))
    val innerSep = data(InnerSep)
    val width = data(MinimumWidth).max(textRect.x + innerSep)
    val height = data(MinimumHeight).max(textRect.y + innerSep)
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
      fill <-- data.map(d =>
        if d.get(Hovered).isDefined then "lightgrey" else d(Fill)
      ),
      stroke <-- data.map(_(Stroke)),
      style <-- data.map(_.get(Style).getOrElse(""))
    )
  }

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

  def apply() = new Rect(defaults)

  def apply(props: PropMap) = new Rect(defaults ++ props)

}
