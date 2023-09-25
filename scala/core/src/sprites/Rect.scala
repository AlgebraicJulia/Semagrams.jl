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
case class Rect(label:Property,val props: PropMap) extends Sprite {
  import Rect._

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

    val text = Sprite.innerText(data)
    
    val box = rect(
      L.onClick --> printObs("box"),
      geomUpdater(data),
      styleUpdater(data),
    )

    val bg = image(
      cls := "disc-bg",
      href <-- data.map(_(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
      Rect.geomUpdater(data)
    )


    val root = g(
      box,
      text,
      bg,
      MouseEvents.handlers(ent, eventWriter),
    )

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

  override def center(_subent: Entity, data: ACSet) = 
    data.props.get(Center)

  override def toTikz(p: Part, data: ACSet, visible: Boolean = true) = tikzNode(
    "rectangle",
    p.tikzName,
    data.props.get(Center).getOrElse(Complex(0, 0)),
    data.props(Content)
      .flatMap(_ match
        case '\n' => "\\\\"
        case ch   => ch.toString()
      ),
    visible
  )

}

object Rect {
  def geom(data: PropMap): (Complex, Complex) = {
    val props = defaults ++ data
    val textRect = boxSize(
      props(Content), 
      props(FontSize),
      split = true
    )
    val center = props.get(Center).getOrElse(Complex(100, 100))
    val innerSep = props(InnerSep)
    val width = props(MinimumWidth).max(textRect.x + innerSep)
    val height = props(MinimumHeight).max(textRect.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater(updates: L.Signal[PropMap]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    val props = data.map(defaults ++ _)
    List(
      fill <-- props.map(d =>
        if d.get(Hovered).isDefined then "lightgrey" else d(Fill)
      ),
      stroke <-- props.map(_(Stroke)),
      style <-- props.map(_(Style))
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
    + (Style,"")

  def apply() = new Rect(Content,defaults)
  def apply(props:PropMap) = new Rect(Content,defaults ++ props)
  def apply(label:Property) = new Rect(label,defaults)
  def apply(label:Property,props: PropMap) = new Rect(label,defaults ++ props)

}
