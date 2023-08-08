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
case class Arrow(props: PropMap) extends Sprite {
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

  def curvedPath(s: Complex, e: Complex, bend: Double): Seq[Path.Element] = {
    import Path.Element._
    val rot = Complex(0, bend).exp
    val λ = 1.0 / 4
    val cs = rot * (λ * (e - s)) + s
    val ce = rot.cong * (λ * (s - e)) + e
    Seq(MoveTo(s), Cubic(cs, ce, e))
  }

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {
    val data = updates.map(props ++ _.props)
    val $p = updates.map(props ++ _.props)
    val arrow = path(
      pathElts <-- data.map(p => curvedPath(p(Start), p(End), p(Bend))),
      stroke <-- $p.map(p =>
        if p.get(Hovered).isDefined then "lightgrey" else p(Stroke)
      ),
      strokeDashArray <-- data.map(_(StrokeDasharray)),
      fill := "none",
      markerEnd := "url(#arrowhead)",
      pointerEvents := "none"
    )
    val handle = path(
      fill := "white",
      opacity := "0",
      pathElts <-- data.map(p => blockPath(p(Start), p(End), 5, p(Bend))),
      pointerEvents <-- data.map(p =>
        if p(Interactable) then "auto" else "none"
      ),
      MouseEvents.handlers(ent, eventWriter)
    )
    g(arrow, handle)
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
      val b = 40 * data.props(Bend)

      val endpts =
        s"\\path ($s) to[bend left={$b}] node[pos=00](a@$s){} node[pos=1](b@$t){} ($t);\n"
      val arrow =
        s"\\draw[->] (a@$s) to[bend left={$b}] (b@$t);\n"

      endpts + arrow

}

object Arrow {
  val props = PropMap()
    + (Stroke, "black")
    + (Bend, 0)
    + (StrokeDasharray, "none")
    + (Interactable, true)

  def apply() = new Arrow(props)
}
