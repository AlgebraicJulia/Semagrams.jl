package semagrams.sprites

import com.raquo.laminar.api.L.svg.{!= as neq,text as Ltext,_}
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
// import semagrams.{text as sematext,_}

import semagrams.util.Complex.{im}

import Math.{log,E}
import upickle.default.ReadWriter


enum WireProp[T: ReadWriter] extends Property {
  case StartDir extends WireProp[Complex]
  case EndDir extends WireProp[Complex]
  case WireLabel extends WireProp[String]
  case LabelAnchor extends WireProp[Double]
  case LabelOffset extends WireProp[Complex]

  type Value = T
  val rw = summon[ReadWriter[T]]
}

export WireProp._

case class Wire() extends Sprite {
  def exAt(p: Complex,d:Double=5.0) = {
    import Path.Element._

    Seq(
      MoveTo(p+d*(1+im)),
      LineTo(p-d*(1+im)),
      MoveTo(p+d*(1-im)),
      LineTo(p-d*(1-im)),
      MoveTo(p)
    )
  }

  def curvedPath(z1: Complex, z2: Complex, dz1: Complex, dz2: Complex, bend: Double=1): Seq[Path.Element] = {
    import Path.Element._
    Seq(MoveTo(z1),Cubic(z1 + bend * dz1, z2 + bend * dz2, z2))
  }

  def blockPath(
    z1: Complex, z2: Complex,
    dz1: Complex, dz2: Complex,
    d: Double, bend: Double): Seq[Path.Element] = {
    import Path.Element._

    val crv = Cubic(z1 + bend*dz1, z2 + bend*dz2, z2)

    val ts = (0 to 100).map(_ * .01)
    val p1 = ts.map(t => crv.pos(z1,t) + d * crv.dir(z1,t)*Complex(0,1))
    val p2 = ts.map(t => crv.pos(z1,t) + d * crv.dir(z1,t)*Complex(0,-1)).reverse

    Seq(MoveTo(p1.head))
      ++ p1.tail.map(LineTo(_))
      ++ Seq(LineTo(p2.head))
      ++ p2.tail.map(LineTo(_))
      ++ Seq(ClosePath)
  }

  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    def s(p:PropMap) = p(Start)
    def t(p:PropMap) = p(End)
    def ds(p:PropMap): Complex = p.get(StartDir).getOrElse(10.0)
    def dt(p:PropMap): Complex = p.get(EndDir).getOrElse(-10.0)
    def b(p:PropMap) = p.get(Bend).getOrElse(10.0)

    def ppath(p:PropMap) = curvedPath(s(p),t(p),ds(p),dt(p),b(p))

    val anchor = p.get(LabelAnchor).getOrElse(.5)
    val offset = p.get(LabelOffset).getOrElse(10*im)

    def labelPos(p:PropMap) = {
      val crv = ppath(p)(1)
      crv.pos(s(p),anchor) + offset * crv.dir(s(p),anchor)
    }

    def label(p:PropMap) = p.get(WireLabel).getOrElse("")
    def fontsize(p:PropMap) = p.get(FontSize).getOrElse(16.0)
    def pstroke(p:PropMap) = p.get(Stroke).getOrElse("black")

    val txt = L.svg.text(
      xy <-- $p.map(labelPos),
      L.svg.tspan(
        L.child <-- $p.map(p => L.textToNode(label(p))),
        textAnchor := "middle",
        dominantBaseline := "central",
        style := "user-select: none"
      ),
      fontSize <-- $p.map(fontsize(_).toString())
    )

    val wire = path(
      pathElts <-- $p.map(ppath),
      stroke <-- $p.map(pstroke),
      fill := "none",
      style := "user-select: none",
      pointerEvents := "none"
    )
    val handle = path(
      pathElts <-- $p.map(
        p => blockPath(s(p),t(p),ds(p),dt(p),3,b(p))
      ),
      fill := "white",
      opacity := ".0",
      stroke := "none",
      style := "user-select: none",
      pointerEvents := "none"
    )

    attachHandlers(ent, handle)
    g(wire, handle, txt)
  }
}
