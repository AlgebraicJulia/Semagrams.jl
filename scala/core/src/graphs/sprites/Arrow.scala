package semagrams.graphs

import semagrams._
import semagrams.util._
import semagrams.rendering._
import semagrams.state._

import com.raquo.laminar.api.L.svg.{path as svgpath, _}
import com.raquo.laminar.api._

/** A basic sprite used for edges, which looks up the `Start` and `End`
  * properties to see where to start and end, has an arrow head pointing towards
  * the end, and curves according to `Bend`.
  *
  * It also has an invisible "handle", which is thicker than the arrow and used
  * for mouse events, because otherwise it would be very annoying to mouse over
  * the arrow.
  */
case class Arrow[D: PartData](label: Property, data: D) extends Sprite[D] {

  val defaultProps = Arrow.defaultProps
  val requiredProps = Seq(Start, End)

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

  def blockPath(
      s: Option[Complex],
      e: Option[Complex],
      d: Double,
      bend: Option[Double]
  ): Seq[Path.Element] = blockPath(
    s.getOrElse(Complex(100, 100)),
    e.getOrElse(Complex(100, 100)),
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
  def curvedPath(
      s: Option[Complex],
      e: Option[Complex],
      bend: Option[Double]
  ): Seq[Path.Element] = curvedPath(
    s.getOrElse(Complex(100, 100)),
    e.getOrElse(Complex(200, 200)),
    bend.getOrElse(2.0)
  )

  def present(
      ent: Part,
      init: D,
      updates: L.Signal[D],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {
    val props =
      updates.map(d => Arrow.defaultProps ++ data.getProps() ++ d.getProps())

    def ppath(p: PropMap) = curvedPath(p.get(Start), p.get(End), p.get(Bend))

    def labelStr(p: PropMap) =
      if p.contains(label) then p(label).toString else p(PathLabel)
    def labelPos(p: PropMap) =
      val pathElt = ppath(p)((ppath(p).length * p(LabelAnchor)).toInt)
      pathElt.pos(p(Start), p(LabelAnchor)) -
        Complex.im * p(LabelOffset) * pathElt.dir(p(Start), p(LabelAnchor))

    def pathStroke(p: PropMap) =
      if p.contains(Selected)
      then (2 * p(StrokeWidth)).toString
      else p(StrokeWidth).toString

    val arrow = svgpath(
      pathElts <-- props.map(ppath),
      stroke <-- props.map(_(Stroke).toString),
      strokeDashArray <-- props.map(_(StrokeDasharray)),
      strokeWidth <-- props.map(pathStroke),
      fill := "none",
      markerEnd := "url(#arrowhead)",
      pointerEvents := "none"
    )

    val text = L.svg.text(
      xy <-- props.map(labelPos),
      L.children <-- props.map { p =>
        val splits = splitString(labelStr(p)).zipWithIndex
        val len = splits.length
        splits.toSeq.map((str, line) =>
          L.svg.tspan(
            L.textToTextNode(str),
            textAnchor := "middle",
            x <-- props.map(p => labelPos(p).x.toString()),
            y <-- props.map(p =>
              (labelPos(p).y + p(FontSize) * (line + 1 - len / 2.0)).toString()
            ),
            style := "user-select: none"
          )
        )
      },
      fontSize <-- props.map(_(FontSize).toString),
      pointerEvents <-- props.map(p =>
        if p(Interactable) then "auto" else "none"
      )
    )

    val handle = svgpath(
      fill <-- props.map(p => if p.contains(Highlight) then "red" else "white"),
      opacity := "0.1",
      pathElts <-- props.map(p =>
        blockPath(p.get(Start), p.get(End), 5, p.get(Bend))
      ),
      pointerEvents <-- props.map(p =>
        if p(Interactable) then "auto" else "none"
      ),
      MouseEvents.handlers(ent, eventWriter)
    )
    g(arrow, handle, text)
  }

  override def toTikz(e: Part, props: D, visible: Boolean = true) =
    if !visible
    then ""
    else
      val p = props.getProps()
      val s = p.get(TikzStart).getOrElse {
        println(s"Missing TikzStart")
        ""
      }
      val t = p.get(TikzEnd).getOrElse {
        println(s"Missing TikzEnd")
        ""
      }
      val b = 40 * p.get(Bend).getOrElse(0.0)

      val endpts =
        s"\\path ($s) to[bend left={$b}] node[pos=00](a@$s){} node[pos=1](b@$t){} ($t);\n"
      val arrow =
        s"\\draw[->] (a@$s) to[bend left={$b}] (b@$t);\n"

      endpts + arrow

}

object Arrow {
  val defaultProps = PropMap()
    + (Stroke, RGB("black"))
    + (StrokeWidth, 1)
    + (Bend, 0)
    + (StrokeDasharray, "none")
    + (Interactable, true)
    + (LabelAnchor, 0.5)
    + (LabelOffset, 10.0)
    + (PathLabel, "")
    + (FontSize, 12)

  def apply[D: PartData](label: Property): Arrow[D] =
    Arrow[D](label, PartData(defaultProps))
  def apply[D: PartData](): Arrow[D] = Arrow[D](Content)
}
