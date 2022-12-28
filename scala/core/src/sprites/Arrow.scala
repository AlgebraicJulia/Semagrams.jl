package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

import Math.{log,E}

import upickle.default._

enum ArrowProp[T: ReadWriter] extends Property:
  case Loop extends ArrowProp[Complex]
  type Value = T
  val rw = summon[ReadWriter[T]]
export ArrowProp._


case class Arrow() extends Sprite {
  def blockPath(s:Complex,crv:Path.Element,d:Double): Seq[Path.Element] = {
    import Path.Element._
    
    val ts = (0 to 100).map(_*.01)
    val p1 = ts.map(t => crv.pos(s,t) + d * crv.dir(s,t)*Complex(0,1))
    val p2 = ts.map(t => crv.pos(s,t) + d * crv.dir(s,t)*Complex(0,-1)).reverse

    Seq(MoveTo(p1.head)) 
      ++ p1.tail.map(LineTo(_))
      ++ Seq(LineTo(p2.head))
      ++ p2.tail.map(LineTo(_))
      ++ Seq(ClosePath)
  }

  def curvedPath(s: Complex, e: Complex, bend: Double): Seq[Path.Element] = {
    import Path.Element._
    val rot = Complex(0, bend).exp
    val cs = rot * (s * (-1 / 4) + e * (1 / 4)) + s
    val ce = rot.cong * (s * (1 / 4) + e * (-1 / 4)) + e
    Seq(MoveTo(s), Cubic(cs, ce, e))
  }

  def loopPath(s: Complex, e: Complex,ctr: Complex,bend:Double): Seq[Path.Element] =
    import Path.Element._

    Seq(MoveTo(s),
      Cubic(
        s + (1 + 5*bend)*(s - ctr), 
        e + (1 + 5*bend)*(e - ctr),
        e
      ),
    )

  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    def ppath(p:PropMap) = p.get(Loop) match
      case Some(z) => loopPath(p(Start),p(End),z,p(Bend))
      case None => curvedPath(p(Start),p(End),p(Bend))
    
    val d = 3
    val arrow = path(
      pathElts <-- $p.map(ppath),
      stroke <-- $p.map(_(Stroke)),
      strokeDashArray <-- $p.map(_.get(StrokeDasharray).getOrElse("none")),
      fill := "none",
      markerEnd := "url(#arrowhead)"
    )
    val handle = path(
      pathElts <-- $p.map(
        p => blockPath(p(Start),ppath(p)(1),d)
      ),
      fill := "white",
      opacity := "0"
    )

    
    val root = g(arrow, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

  def boundaryNormal(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

}
