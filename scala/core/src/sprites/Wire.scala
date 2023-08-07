package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets._

import semagrams.util.Complex.{im}

import upickle.default.ReadWriter

enum WireProp[T: ReadWriter] extends Property {
  case StartDir extends WireProp[Complex]
  case EndDir extends WireProp[Complex]
  case LabelAnchor extends WireProp[Double]
  case LabelOffset extends WireProp[Complex]
  case TikzStart extends WireProp[String]
  case TikzEnd extends WireProp[String]

  type Value = T
  val rw = summon[ReadWriter[T]]
}

export WireProp._

/** A sprite used for wires. Similar to [[Arrow]], except this one is a spline
  * where the beginning and the end are both horizontal, and it has no
  * arrowhead.
  */
case class Wire() extends Sprite {

  def exAt(p: Complex, d: Double = 5.0) = {
    import Path.Element._

    Seq(
      MoveTo(p + d * (1 + im)),
      LineTo(p - d * (1 + im)),
      MoveTo(p + d * (1 - im)),
      LineTo(p - d * (1 - im)),
      MoveTo(p)
    )
  }

  def curvedPath(
      z1: Complex,
      z2: Complex,
      dz1: Complex,
      dz2: Complex,
      bend: Double = 1
  ): Seq[Path.Element] = {
    import Path.Element._
    Seq(MoveTo(z1), Cubic(z1 + bend * dz1, z2 + bend * dz2, z2))
  }

  def blockPath(
      z1: Complex,
      z2: Complex,
      dz1: Complex,
      dz2: Complex,
      d: Double,
      bend: Double
  ): Seq[Path.Element] = {
    import Path.Element._

    val crv = Cubic(z1 + bend * dz1, z2 + bend * dz2, z2)

    val ts = (0 to 100).map(_ * .01)
    val p1 = ts.map(t => crv.pos(z1, t) + d * crv.dir(z1, t) * Complex(0, 1))
    val p2 =
      ts.map(t => crv.pos(z1, t) + d * crv.dir(z1, t) * Complex(0, -1)).reverse

    Seq(MoveTo(p1.head))
      ++ p1.tail.map(LineTo(_))
      ++ Seq(LineTo(p2.head))
      ++ p2.tail.map(LineTo(_))
      ++ Seq(ClosePath)
  }

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {

    val data = updates.map(init.props ++ _.props)
    def s(p: PropMap) = p
      .get(Start)
      .getOrElse(
        throw msgError(s"present $ent missing `Start`")
      )
    def t(p: PropMap) = p
      .get(End)
      .getOrElse(
        throw msgError(s"present $ent missing `End`")
      )
    def ds(p: PropMap): Complex = p.get(StartDir).getOrElse(-10.0)
    def dt(p: PropMap): Complex = p.get(EndDir).getOrElse(-10.0)
    def b(p: PropMap) = p.get(Bend).getOrElse(10.0)

    def ppath(p: PropMap) = curvedPath(s(p), t(p), ds(p), dt(p), b(p))

    def anchor(p: PropMap) = p.get(LabelAnchor).getOrElse(.5)
    def offset(p: PropMap) = p.get(LabelOffset).getOrElse(10 * im)

    def labelPos(p: PropMap) = {
      val crv = ppath(p)(1)
      crv.pos(s(p), anchor(p)) + offset(p) * crv.dir(s(p), anchor(p))
    }

    def label(p: PropMap): String = p.get(Content).getOrElse("")
    def fontsize(p: PropMap): Double = p.get(FontSize).getOrElse(16.0)
    def pstroke(p: PropMap) = p.get(Stroke).getOrElse("black")


    val text = L.svg.text(
      xy <-- data.map(labelPos),
      L.children <-- data.map { p =>
        val splits = label(p).split('\n').zipWithIndex
        val len = splits.length
        splits.toSeq.map((str, line) =>
          L.svg.tspan(
            L.textToTextNode(str),
            textAnchor := "middle",
            x <-- data.map(p => labelPos(p).x.toString()),
            y <-- data.map(p =>
              (labelPos(p).y + fontsize(p) * (line + 1 - len / 2.0)).toString()
            ),
            style := "user-select: none"
          )
        )
      },
      fontSize <-- data.map(fontsize(_).toString()),
      pointerEvents := "none"
    )

    val wire = path(
      pathElts <-- data.map(ppath),
      stroke <-- data.map(p =>
        if p.get(Hovered).isDefined then "lightgrey" else pstroke(p)
      ),
      fill := "none",
      style := "user-select: none",
      pointerEvents := "none"
    )

    val handle = path(
      pathElts <-- data.map(p => blockPath(s(p), t(p), ds(p), dt(p), 7, b(p))),
      fill := "blue",
      opacity := ".1",
      stroke := "none",
      style := "user-select: none",
      pointerEvents <-- data.map(p =>
        if p.get(Interactable) != Some(false) then "auto" else "none"
      )
    )

    g(wire, handle, text)
  }

  override def toTikz(w: Part, data: ACSet, visible: Boolean = true) =
    if !visible
    then ""
    else
      val s_str = data.props
        .get(TikzStart)
        .getOrElse(
          throw msgError(s"missing property `TikzStart`")
        )
      val t_str = data.props
        .get(TikzEnd)
        .getOrElse(
          throw msgError(s"missing property `TikzStart`")
        )

      val labelStr = data.props
        .get(Content)
        .map(label =>
          s" node[above,midway,align=center]{${tikzLabel(label, "footnotesize")}}"
        )
        .getOrElse("")

      s"\\draw ($s_str.center) to[out=0,in=180] $labelStr ($t_str.center);\n"

}
