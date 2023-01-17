package semagrams.sprites

import semagrams._
import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._

def ACSetEntitySource[S: IsSchema](
    ob: Ob,
    sprite: Sprite
) =
  EntitySource[ACSet[S]]((acs, _m) =>
    acs.parts(ob).map(i => (i, sprite, acs.props(i)))
  )

def edgeProps[E1 <: Entity, E2 <: Entity](
    src: PValue[E1],
    tgt: PValue[E2]
)(_e: Entity, p: PropMap, m: EntityMap): PropMap = {
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s.map(i => m(i)._2(Center)).getOrElse(p(Start))
  val tpos = t.map(i => m(i)._2(Center)).getOrElse(p(End))
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = Complex(0, bend).exp
  val start = s
    .map(m(_))
    .map((sprite, props) => sprite.boundaryPt(MainHandle, props, -dir * rot)).get
    .getOrElse(spos)
  val nd = t
    .map(m(_))
    .map((sprite, props) => sprite.boundaryPt(MainHandle, props, dir * rot.cong)).get
    .getOrElse(tpos)
  PropMap() + (Start, start) + (End, nd)
}

def ACSetEdgeSource[S: IsSchema, E1 <: Entity, E2 <: Entity](
    ob: Ob,
    src: PValue[E1],
    tgt: PValue[E2],
    sprite: Sprite
) = ACSetEntitySource[S](ob, sprite).addPropsBy(edgeProps(src, tgt))
