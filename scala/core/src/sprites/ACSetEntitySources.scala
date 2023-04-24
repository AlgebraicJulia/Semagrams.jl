package semagrams.sprites

import semagrams._
import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._

/** An [[EntitySource]] that extracts all parts of type `ob` from an ACSet and
  * pairs them with `sprite` along with their subacset
  */
def ACSetEntitySource(
    ob: Ob,
    sprite: Sprite
) =
  EntitySource[ACSet]((acs, _m) =>
    acs.parts(ROOT, ob).map({ case (i, acs) => 
      (i, sprite, acs) })
  )

/** Find the point on the boundary in direction `dir` of the sprite
  * corresponding to `p`, by looking up the sprite/data in `m`
  */
def findBoundary(p: Part, m: EntityMap, dir: Complex): Option[Complex] = for {
  ((sprite, acs), subp) <- p.path match {
    case Nil             => None
    case (x, id) :: rest => m.get(Part(Seq((x, id)))).map((_, Part(rest)))
  }
  bp <- sprite.boundaryPt(subp, acs, dir)
} yield bp

/** Find the center of the sprite corresponding to `p`, by looking up the
  * sprite/data in `m`
  */
def findCenter(p: Part, m: EntityMap): Option[Complex] = 
  for {
  ((sprite, acs), subp) <- p.path match {
    case Nil             => None
    case (x, id) :: rest => m.get(Part(Seq((x, id)))).map((_, Part(rest)))
  }
  c <- sprite.center(subp, acs)
} yield c

/** Compute the properties (i.e. Start and End) for an edge, using the top-level
  * properties in `acs` and the other sprites in `m`.
  *
  * If `Start`/`End` are already set, it uses those, otherwise it looks up a
  * point on the boundary of the sprite corresponding to the `src`/`tgt` of the
  * edge.
  *
  * Need to update this to look up the sprite for just the first part of
  * src/tgt, and then pass the rest of the path of the part into a method on
  * that sprite.
  */
def edgeProps(
    src: Hom,
    tgt: Hom
)(_e: Entity, acs: ACSet, m: EntityMap): PropMap = {
  val p = acs.props
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s.flatMap(findCenter(_, m)).getOrElse(p(Start))
  val tpos = t.flatMap(findCenter(_, m)).getOrElse(p(End))
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = Complex(0, bend).exp
  val start = s
    .flatMap(findBoundary(_, m, -dir * rot))
    .getOrElse(spos)
  val nd = t
    .flatMap(findBoundary(_, m, dir * rot.cong))
    .getOrElse(tpos)
  PropMap() + (Start, start) + (End, nd)
}

/** Like [[ACSetEntitySource]], but then also computes the edge properties using
  * [[edgeProps]]
  */
def ACSetEdgeSource(
    ob: Ob,
    src: Hom,
    tgt: Hom,
    sprite: Sprite
) = ACSetEntitySource(ob, sprite).addPropsBy(edgeProps(src, tgt))


