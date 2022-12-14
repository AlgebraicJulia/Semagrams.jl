package semagrams.sprites

import com.raquo.laminar.api.L.svg.{!= as neq,_}
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._

import Math.{log,E}

import upickle.default._

enum WireProp[T: ReadWriter] extends Property:
  case StartDir extends WireProp[Complex]
  case EndDir extends WireProp[Complex]

  type Value = T
  val rw = summon[ReadWriter[T]]
export WireProp._

case class Wire() extends Sprite {

  def exAt(p: Complex) = 
    import Path.Element._
    Seq(
      MoveTo(p+Complex(0,5)),
      LineTo(p+Complex(0,-5)),
      MoveTo(p+Complex(5,0)),
      LineTo(p+Complex(-5,0)),
    )

  

  def curvedPath(z1: Complex, z2: Complex, dz1: Complex, dz2: Complex, bend: Double=1): Seq[Path.Element] = {
    import Path.Element._
    Seq(MoveTo(z1),Cubic(z1 + bend * dz1, z2 + bend * dz2, z2))
  }


  
  def blockPath(
    z1: Complex, z2: Complex,
    dz1: Complex, dz2: Complex,
    d: Double, bend: Double): Seq[Path.Element] = {
    import Path.Element._

    val p0 = Cubic(z1 + bend*dz1, z2 + bend*dz2, z2)

    val ts = (0 to 100).map(_ * .01)
    val p1 = ts.map(t => p0.pos(z1,t) + d * p0.dir(z1,t)*Complex(0,1))
    val p2 = ts.map(t => p0.pos(z1,t) + d * p0.dir(z1,t)*Complex(0,-1)).reverse

    Seq(MoveTo(p1.head)) 
      ++ p1.tail.map(LineTo(_))
      ++ Seq(LineTo(p2.head))
      ++ p2.tail.map(LineTo(_))
      ++ Seq(ClosePath)
  }



  def present(
      ent: Entity,
      p: PropMap,
      $p: L.Signal[PropMap]
  ): RenderedSprite = {
    val wire = path(
      pathElts <-- $p.map(
        p => curvedPath(p(Start), p(End), p(StartDir), p(EndDir),p(Bend))
      ),
      stroke <-- $p.map(_(Stroke)),
      fill := "none",
      // markerEnd := "url(#arrowhead)"
    )
    val handle = path(
      pathElts <-- $p.map(
        p => blockPath(p(Start), p(End), p(StartDir), p(EndDir), 3, p(Bend))
      ),
      fill := "green",
      opacity := ".3",
      stroke := "none",
    )

    
    val root = g(wire, handle)
    RenderedSprite(root, Map(MainHandle -> handle))
  }

  def boundaryPt(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)

  def boundaryNormal(data: PropMap, dir: Complex): Complex =
    Complex(0, 0)
}
