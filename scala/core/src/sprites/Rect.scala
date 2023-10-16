package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
// import semagrams.acsets._
import semagrams.acsets.abstr._

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))

/** A sprite for geometric rectangles
  *
  * Resizes automatically corresponding to its content.
  */
case class Rect[D:PartData](label:Property,val init: D) extends Sprite[D] {
  import Rect._

  def setLabel: D => D = Sprite.setContent(label)
  def present(
      ent: Part,
      init: D,
      updates: L.Signal[D],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {
    val data = updates
      .map(init.merge(_))
      .map(setLabel)

    val text = Sprite.innerText(data)
    
    val box = rect(
      geomUpdater(data),
      styleUpdater(data),
    )

    val bg = image(
      Rect.geomUpdater(data),
      Rect.bgUpdater(data)
    )



    val root = g(
      box,
      text,
      bg,
      MouseEvents.handlers(ent, eventWriter),
    )

    root
  }

  override def boundaryPt(data: D, dir: Complex, subparts:Seq[Part] = Seq()) = {
    // Normalize to first quadrant
    val pm = init.merge(data)
    val (_, boxdims) = geom(pm)
    val os = pm.getProp(OuterSep)
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
      Complex(q1pt.x * dir.x.sign, q1pt.y * dir.y.sign) + data.getProps()
        .get(Center)
        .getOrElse(Complex(100, 100))
    )
  }

  override def bbox(data: D, subparts: Seq[Part] = Seq()) = {
    val (pos, dims) = geom(init.merge(data))
    Some(BoundingBox(pos, dims))
  }

  override def center(data: D, subparts: Seq[Part] = Seq()) = 
    data.tryProp(Center)

  override def toTikz(p: Part, data: D, visible: Boolean = true) = tikzNode(
    "rectangle",
    p.tikzName,
    data.tryProp(Center).getOrElse(Complex(0, 0)),
    data.getProp(Content)
      .flatMap(_ match
        case '\n' => "\\\\"
        case ch   => ch.toString()
      ),
    visible
  )

}

object Rect {
  def geom[D:PartData](data: D): (Complex, Complex) = {
    val props = defaults.merge(data)
    val textRect = boxSize(
      props.getProp(Content), 
      props.getProp(FontSize),
      split = true
    )
    val center = props.tryProp(Center).getOrElse(Complex(100, 100))
    val innerSep = props.getProp(InnerSep)
    val width = props.getProp(MinimumWidth).max(textRect.x + innerSep)
    val height = props.getProp(MinimumHeight).max(textRect.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater[D:PartData](updates: L.Signal[D]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater[D:PartData](data: L.Signal[D]) = {
    val props = data.map(defaults.merge(_))
    List(
      fill <-- props.map(d =>
        if d.hasProp(Hovered) then "lightgrey" else d.getProp(Fill)
      ),
      stroke <-- props.map(_.getProp(Stroke)),
      style <-- props.map(_.getProp(Style))
    )
  }
  def bgUpdater[D:PartData](data: L.Signal[D]) = {
    val props = data.map(defaults.merge(_))
    List(
      cls := "disc-bg",
      href <-- props.map(_.getProp(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
    )
  }

  def defaults[D:PartData] = PartData[D](
    PropMap()
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
  )

  def apply() = new Rect(Content,defaults)
  def apply(props:PropMap) = new Rect(Content,defaults ++ props)
  def apply(label:Property) = new Rect(label,defaults)
  def apply(label:Property,props: PropMap) = new Rect(label,defaults ++ props)

}
