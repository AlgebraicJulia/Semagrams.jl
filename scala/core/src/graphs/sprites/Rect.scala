package semagrams.graphs

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams.rendering._
import semagrams._
import semagrams.state._


/** A sprite for geometric rectangles
  *
  * Resizes automatically corresponding to its content.
  */
case class Rect[D:PartData](label:Property,val rectInit: D) extends Sprite[D]:
  import Rect._

  def defaultProps = Sprite.defaultProps
  def requiredProps = Seq(Center)

  def setLabel: D => D = Sprite.setContent(label)
  def present(
      ent: Part,
      init: D,
      updates: L.Signal[D],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {

    val initData = rectInit.merge(init)
    val data = updates
      .map(initData.merge(_))
      .map(setLabel)

    val text = data.map(Sprite.innerText)
    
    val box = rect(
      cls := "rect-box",
      geomUpdater(data),
      styleUpdater(data),
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

  override def boundaryPt(data: D, dir: Complex, subparts:Seq[Part] = Seq()) = {
    /* Normalize to first quadrant */
    val pm = rectInit.merge(data)
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
    val (pos, dims) = geom(rectInit.merge(data))
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



object Rect:
  import Sprite.defaultProps

  def geom[D:PartData](data: D): (Complex, Complex) = {
    val props = data.softSetProps(defaultProps)
    val textRect = boxSize(
      props.getProp(Content), 
      props.getProp(FontSize),
      split = true
    )
    val center = props.tryProp(Center)
      .getOrElse(throw msgError(s"missing center in $data"))
    val innerSep = props.getProp(InnerSep)
    val width = props.getProp(MinimumWidth)
      .max(textRect.x + innerSep)
    val height = props.getProp(MinimumHeight)
      .max(textRect.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater[D:PartData](updates: L.Signal[D]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater[D:PartData](updates: L.Signal[D]) = {
    val props = updates.map(_.softSetProps(defaultProps))
    
    List(
      fill <-- props.map(d =>
        if d.hasProp(Highlight) 
        then d.getProp(Fill).lighten(.5).toString
        else d.getProp(Fill).toString
      ),
      strokeWidth <-- props.map(d =>
        if d.hasProp(Selected) 
        then "3"
        else "1"
      ),
      stroke <-- props.map(data => data.tryProp(Stroke).getOrElse(
        RGB("black")
      ).toString),
      style <-- props.map(_.getProp(Style))
    )
  }
  def bgUpdater[D:PartData](data: L.Signal[D]) = {
    val props = data.map(_.softSetProps(defaultProps))
    List(
      cls := "disc-bg",
      href <-- props.map(_.getProp(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
    )
  }

  def apply[D:PartData]() = new Rect[D](Content,PartData(defaultProps))
  def apply[D:PartData](data:D) = new Rect[D](Content,data.softSetProps(defaultProps))
  def apply[D:PartData](label:Property) = new Rect[D](label,PartData(defaultProps))
  def apply[D:PartData](label:Property,data: D) = new Rect(label,data.softSetProps(defaultProps))


