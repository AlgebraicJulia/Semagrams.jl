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
  def blockPath(
      s: Complex,
      e: Complex,
      d: Double,
      bend: Double
  ): Seq[Path.Element] = {
    import Path.Element._
    val dz = (if s == e then Complex(0,1) else s - e).normalize * Complex(0, 1) * d
    curvedPath(s + dz, e + dz, bend)
      ++ Seq(LineTo(e - dz))
      ++ curvedPath(e - dz, s - dz, -bend)
      ++ Seq(LineTo(s + dz), ClosePath)
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

    Seq(
      MoveTo(s),
      Cubic(
        s + (1 + 5*bend)*(s - ctr), 
        e + (1 + 5*bend)*(e - ctr),
        e
      ),
    )


  def loopBlock(s: Complex, e: Complex, d: Double,
      ctr: Complex, bend: Double): Seq[Path.Element] = {
    import Path.Element._
    val dir = if e == s then
      Complex(d,0)
    else
      d*(e - s).normalize
      
    val mid = s + (e - s)/2.0
    val ctrl = dir * Complex(0,1 + bend)
    
    Seq(
      MoveTo(s + dir),
      Cubic(
        (s + dir) + (1 + 4*bend)*(s + dir - ctr),
        (e - dir) + (1 + 4*bend)*(e - dir - ctr),
        e - dir
      ),
      LineTo(e - dir),
      Cubic(
        (e + dir) + (1 + 6*bend)*(e + dir - ctr),
        (s - dir) + (1 + 6*bend)*(s - dir - ctr),
        s - dir
      ),
      ClosePath
    )
  }


  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    val arrow = path(
      pathElts <-- $p.map(p => 
        p.get(Loop) match
          case Some(z) =>
            loopPath(p(Start),p(End),z,p(Bend))
          case _ => 
            curvedPath(p(Start), p(End), p(Bend))
      ),
      stroke <-- $p.map(_(Stroke)),
      fill := "none",
      markerEnd := "url(#arrowhead)"
    )
    val handle = path(
      pathElts <-- $p.map(p => 
        p.get(Loop) match
          case Some(z) => 
            loopBlock(p(Start), p(End), 2, z, p(Bend))
          case _ =>
            blockPath(p(Start), p(End), 5, p(Bend))
      ),
      fill := "white",
      opacity := ".3"
    )

    
    val root = g(arrow, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

  def boundaryNormal(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

}
