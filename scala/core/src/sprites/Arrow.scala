package semagrams.sprites

import semagrams.acsets._
import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams._
import semagrams.util._

/** A basic sprite used for edges, which looks up the `Start` and `End`
  * properties to see where to start and end, has an arrow head pointing towards
  * the end, and curves according to `Bend`.
  *
  * It also has an invisible "handle", which is thicker than the arrow and used
  * for mouse events, because otherwise it would be very annoying to mouse over
  * the arrow.
  */
case class Arrow(label:Property,props: PropMap) extends Sprite {

  def blockPath(
      s: Complex,
      e: Complex,
      d: Double,
      bend: Double
  ): Seq[Path.Element] = {
    import Path.Element._
    val dz = (s - e).normalize * Complex(0, 1) * d
    curvedPath(s + dz, e + dz, bend)
      ++ Seq(LineTo(e - dz))
      ++ curvedPath(e - dz, s - dz, -bend)
      ++ Seq(LineTo(s + dz), ClosePath)
  }

  def blockPath(s:Option[Complex],e:Option[Complex],d:Double,bend:Option[Double]): Seq[Path.Element] = blockPath(
    s.getOrElse(Complex(100,100)),
    e.getOrElse(Complex(100,100)),
    d,
    bend.getOrElse(2.0)
  )

  def curvedPath(s: Complex, e: Complex, bend: Double): Seq[Path.Element] = {
    import Path.Element._
    val rot = Complex(0, -bend).exp
    val λ = 1.0 / 4
    val cs = rot * (λ * (e - s)) + s
    val ce = rot.cong * (λ * (s - e)) + e
    Seq(MoveTo(s), Cubic(cs, ce, e))
  }
  def curvedPath(s:Option[Complex],e:Option[Complex],bend:Option[Double]): Seq[Path.Element] = curvedPath(
    s.getOrElse(Complex(100,100)),
    e.getOrElse(Complex(200,200)),
    bend.getOrElse(2.0)
  )

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {
    val data = updates.map(Arrow.defaultProps ++ props ++ _.props)

    def ppath(p:PropMap) = curvedPath(p.get(Start), p.get(End), p.get(Bend))
    def hov(p:PropMap) = if p.contains(Hovered) then "lightgrey" else p.get(Stroke).getOrElse("black")
    def labelStr(p:PropMap) = if p.contains(label) then p(label).toString else p(PathLabel)
    def labelPos(p:PropMap) = 
      val pathElt = ppath(p)((ppath(p).length * p(LabelAnchor)).toInt)
      pathElt.pos(p(Start),p(LabelAnchor)) -
        Complex.im * p(LabelOffset) * pathElt.dir(p(Start),p(LabelAnchor))

    
    val arrow = path(
      pathElts <-- data.map(ppath),
      stroke <-- data.map(hov),
      strokeDashArray <-- data.map(_(StrokeDasharray)),
      fill := "none",
      markerEnd := "url(#arrowhead)",
      pointerEvents := "none"
    )
    
    val text = L.svg.text(
      xy <-- data.map(labelPos),
      L.children <-- data.map { p =>
        val splits = splitString(labelStr(p)).zipWithIndex
        val len = splits.length
        splits.toSeq.map((str, line) =>
          L.svg.tspan(
            L.textToTextNode(str),
            textAnchor := "middle",
            x <-- data.map(p => labelPos(p).x.toString()),
            y <-- data.map(p =>
              (labelPos(p).y + p(FontSize) * (line + 1 - len / 2.0)).toString()
            ),
            style := "user-select: none"
          )
        )
      },
      fontSize <-- data.map(_(FontSize).toString),
      pointerEvents := "none"
    )



    val handle = path(
      fill := "red",
      opacity := "0.1",
      pathElts <-- data.map(p => blockPath(p.get(Start), p.get(End), 5, p.get(Bend))),
      pointerEvents <-- data.map(p =>
        if p(Interactable) then "auto" else "none"
      ),
      MouseEvents.handlers(ent, eventWriter)
    )
    g(arrow, handle, text)
  }

  override def toTikz(e: Part, data: ACSet, visible: Boolean = true) =
    if !visible
    then ""
    else
      val s = data.props.get(TikzStart).getOrElse {
        println(s"Missing TikzStart")
        ""
      }
      val t = data.props.get(TikzEnd).getOrElse {
        println(s"Missing TikzEnd")
        ""
      }
      val b = 40 * data.props.get(Bend).getOrElse(0.0)

      val endpts =
        s"\\path ($s) to[bend left={$b}] node[pos=00](a@$s){} node[pos=1](b@$t){} ($t);\n"
      val arrow =
        s"\\draw[->] (a@$s) to[bend left={$b}] (b@$t);\n"

      endpts + arrow

}

object Arrow {
  val defaultProps = PropMap()
    + (Stroke, "black")
    + (Bend, 0)
    + (StrokeDasharray, "none")
    + (Interactable, true)
    + (Start,Complex(100,100))
    + (End,Complex(200,200))
    + (LabelAnchor,0.5)
    + (LabelOffset,10.0)
    + (PathLabel,"")
    + (FontSize,10.0)

  def apply(label:Property=Content,props:PropMap = PropMap()) = new Arrow(label,defaultProps ++ props)
}
