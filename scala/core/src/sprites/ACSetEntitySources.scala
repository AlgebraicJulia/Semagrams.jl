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
): EntitySource[ACSet] =
  EntitySource[ACSet]((acs, _m) =>
    acs
      .parts(ROOT, ob)
      .map({ case (i, acs) =>
        (i, sprite, acs)
      })
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
      case Nil => None
      case (x, id) :: rest =>
        m.get(Part(Seq((x, id)))).map((_, Part(rest)))
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

  val tikzProps = (s, t) match
    case (Some(p), Some(q)) =>
      PropMap().set(TikzStart, p.tikzName).set(TikzEnd, q.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, nd)
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

/** Similar to [[edgeProps]]. Computes the position and direction for the ends
  * of a wire from the ports it is connected to, using the top-level properties
  * in `acs` and the other sprites in `m`.
  *
  * If `src`/`tgt` are present, it uses those, otherwise it looks up an explicit
  * `Start`/`End` value in `acs`
  *
  * Takes an optional callback argument `bg: => Part` to relativize the lookup
  * to a variable background for zooming in and out.
  */

def wireProps(
    src: Hom,
    tgt: Hom,
    typeProps: (ACSet, Part) => PropMap,
    dir: Part => Complex = _ => Complex(0, 0),
    bg: => Part = ROOT
)(_e: Entity, acs: ACSet, m: EntityMap): PropMap = {

  val defaultPos = Complex(50, 50)

  val p = acs.props

  val Seq(s, t) = Seq(src, tgt).map(p.get(_))

  val sc = s
    .flatMap(_.diffOption(bg))
    .flatMap(findCenter(_, m))
    .getOrElse(
      p.get(Start)
        .getOrElse(defaultPos)
    )
  val tc = t
    .flatMap(_.diffOption(bg))
    .flatMap(findCenter(_, m))
    .getOrElse(
      p.get(End)
        .getOrElse(defaultPos)
    )

  val Seq(sd, td) =
    Seq(s, t).map(_.flatMap(_.diffOption(bg)).map(dir).getOrElse(Complex(0, 0)))

  val tikzProps = (s, t) match
    case (Some(p), Some(q)) =>
      PropMap()
        .set(TikzStart, p.tikzName)
        .set(TikzEnd, q.tikzName)
    case _ => PropMap()

  (acs.props ++ typeProps(acs, _e.asInstanceOf[Part]))
    .set(Start, sc)
    .set(WireProp.StartDir, sd)
    .set(End, tc)
    .set(WireProp.EndDir, td)
    ++ tikzProps

}
