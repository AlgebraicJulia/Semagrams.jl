package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.util._
import semagrams.acsets.abstr._

/** A Sprite for a geometric Disc shape.
  *
  * Auto-resizes based on the content inside.
  */
case class Disc[D:PartData](label:Property,init: D) extends Sprite[D] {
  import Disc._

  def requiredProps = Seq(Center)
  def defaultProps = Disc.defaultProps


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
    
    val box = circle(
      geomUpdater(data),
      styleUpdater(data)
    )

    val text = Sprite.innerText(data) 
      
    //   L.svg.text(
    //   xy <-- data.map(_.get(Center).getOrElse(Complex(100, 100))),
    //   L.svg.tspan(
    //     L.child <-- data.map(p => L.textToTextNode(p(Content))),
    //     textAnchor := "middle",
    //     dominantBaseline := "central",
    //     style := "user-select: none"
    //   ),
    //   fontSize <-- data.map(_(FontSize).toString)
    // )
    val bg = image(
      cls := "disc-bg",
      href <-- data.map(_.getProp(ImageURL)),
      clipPathAttr := "inset(0% round 50%)",
      pointerEvents := "none",
      Rect.geomUpdater(data),
      /* Prevent selection outside semagram window (on double click) */
      L.svg.style := "user-select:none"
    )

    g(
      box,
      bg,
      text,
      MouseEvents.handlers(ent, eventWriter)
    )
  }

  override def boundaryPt(init: D, dir: Complex, subparts: Seq[Part] = Seq()) = {
    val data = init.softSetProps(defaultProps)
    val rad = radius(data) + data.tryProp(OuterSep)
      .getOrElse(Disc.defaultProps(OuterSep))
    
    data.tryProp(Center).map(dir.normalize * rad + _)
  }

  override def bbox(data: D, subparts: Seq[Part] = Seq()) =
    center(data,subparts).map(
      BoundingBox(_, Complex(2 * radius(init), 2 * radius(init)))
    )

  override def center(data: D, subparts: Seq[Part] = Seq()) =
    data.getProps().get(Center)

  override def toTikz(p: Part, data: D, visible: Boolean = true) = tikzNode(
    "circle",
    p.tikzName,
    data.getProps().get(Center).getOrElse(Complex(0, 0)),
    data.getProps().get(label).getOrElse("").toString
      .flatMap(_ match
        case '\n' => "\\\\"
        case ch   => ch.toString()
      ),
    visible
  )

}

object Disc {

  def radius[D:PartData](data: D): Double = {
    val textBox = boxSize(data.tryProp(Content), data.tryProp(FontSize),split = true)
    val innerSep = data.tryProp(InnerSep).getOrElse(defaultProps(InnerSep))
    val d = data
      .tryProp(MinimumWidth)
      .getOrElse(defaultProps(MinimumWidth))
      .max(textBox.x + innerSep)
      .max(textBox.y + innerSep)
    val r = d / 2
    r
  }

  def geomUpdater[D:PartData](data: L.Signal[D]) = {
    List(
      cxy <-- data.map(_.tryProp(Center).getOrElse(defaultProps(Center))),
      r <-- data.map(radius(_).toString)
    )
  }

  def styleUpdater[D:PartData](data: L.Signal[D]) = {
    List(
      fill <-- data.map(_.getProp(Fill).toString),
      opacity <-- data.map(d => if d.tryProp(Hovered).isDefined then ".8" else "1"),
      stroke <-- data.map(_.tryProp(Stroke).getOrElse(defaultProps(Stroke)).toString),
      style <-- data.map(_.tryProp(Style).getOrElse(defaultProps(Style)))
    )
  }




  val defaultProps = PropMap()
    // + (Center, Complex(100,100))
    + (Content, "")
    + (ImageURL, "")
    + (FontSize, 14)
    + (Fill, RGB(0,0,0))
    + (Stroke, RGB("black"))
    + (InnerSep, 10)
    + (OuterSep, 5)
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)
    + (Style,"")


  def apply() = new Disc(Content,defaultProps)
  def apply(props:PropMap) = new Disc(Content,defaultProps ++ props)
  def apply(label:Property) = new Disc(label,defaultProps)
  def apply(label:Property,props: PropMap) = new Disc(label,defaultProps ++ props)

  def boundaryNormal(data: PropMap, dir: Complex) = dir.normalize

}
