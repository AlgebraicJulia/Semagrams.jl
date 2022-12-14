package semagrams.sprites

import semagrams._
import semagrams.acsets._
import com.raquo.laminar.api.L._

def ACSetEntitySource[S: IsSchema](
  ob: Ob,
  sprite: Sprite
) =
  EntitySource[ACSet[S]](
    (acs,_m) => acs.parts(ob).map(i => (i, (sprite, acs.props(i)))).toMap
  )

def edgeProps(src: Hom, tgt: Hom)(_e: Entity, p: PropMap, m: EntityMap): PropMap = {
  val s = p.get(src).asInstanceOf[Option[Part]]
  val t = p.get(tgt).asInstanceOf[Option[Part]]
  val spos = s.map(i => m(i)._2(Center)).getOrElse(p(Start))
  val tpos = t.map(i => m(i)._2(Center)).getOrElse(p(End))
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = Complex(0, bend).exp
  val start = s.map(m(_))
    .map((sprite, props) => sprite.boundaryPt(props, -dir * rot))
    .getOrElse(spos)
  val nd = t.map(m(_))
    .map((sprite, props) => sprite.boundaryPt(props, dir * rot.cong))
    .getOrElse(tpos)
  PropMap() + (Start, start) + (End, nd)
}

def ACSetEdgeSource[S: IsSchema](
  ob: Ob, src: Hom, tgt: Hom,
  sprite: Sprite
) = ACSetEntitySource[S](ob, sprite).addPropsBy(edgeProps(src, tgt))
