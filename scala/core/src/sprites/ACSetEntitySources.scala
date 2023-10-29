package semagrams.sprites

import semagrams._
import semagrams.acsets.abstr._
import semagrams.util._
import semagrams.acsets.simple.SimpleSchema

/** An [[EntitySource]] that extracts all parts of type `ob` from an ACSet and
  * pairs them with `sprite` along with their subacset
  */
def ACSetEntitySource[D:PartData,A:ACSetWithData[D]](
    sprite: Sprite[D],
    test: Ob => Boolean
): EntitySource[D,A] =
  EntitySource[D,A]((acs, _m) =>
    acs.schema.obs.filter(test)
      .flatMap(ob => acs.getData(ob).toSeq)
      .map( (part, data) =>
        (part, sprite, data)
      )
  )

def ACSetEntitySource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],obs: Ob*): EntitySource[D,A] =
  ACSetEntitySource[D,A](sprite,obs.contains)


/** Find the point on the boundary in direction `dir` of the sprite
  * corresponding to `p`, by looking up the sprite/data in `m`
  */
def findBoundary[D:PartData](p: Part, m: EntityMap[D], dir: Complex,
  subparts:Seq[Part] = Seq()
): Option[Complex] = m.get(p).flatMap( (spr,data) =>
  spr.boundaryPt(data,dir,subparts)

)
  

/** Find the center of the sprite corresponding to `p`, by looking up the
  * sprite/data in `m`
  */
def findCenter[D:PartData](p: Part, m: EntityMap[D],
  subparts:Seq[Part] = Seq()
): Option[Complex] =
  m.get(p).flatMap( (spr,data) =>
    spr.center(data,subparts)  
  )


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
def edgeProps[D:PartData](
    src: PartProp,
    tgt: PartProp
)(_e: Part, data: D, m: EntityMap[D]): PropMap = {
  val p = data.getProps()
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s.flatMap(findCenter(_, m)).getOrElse(p.get(Start).getOrElse(Complex(100,100)))
  val tpos = t.flatMap(findCenter(_, m)).getOrElse(p.get(End).getOrElse(Complex(100,100)))
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = Complex(0, -bend).exp
  val start = s
    .flatMap(findBoundary(_, m, -dir * rot))
    .getOrElse(spos)
  val theend = t
    .flatMap(findBoundary(_, m, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, t) match
    case (Some(p), Some(q)) =>
      PropMap().set(TikzStart, p.tikzName).set(TikzEnd, q.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, theend)
}

/** Like [[ACSetEntitySource]], but then also computes the edge properties using
  * [[edgeProps]]
  */
def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](
    sprite: Sprite[D],
    edef: PartialFunction[Ob,(PartProp,PartProp)]
): EntitySource[D,A] = 
  ACSetEntitySource[D,A](sprite,edef.isDefinedAt)
    .addPropsBy( (part,data,emap) =>
      edgeProps[D].tupled(edef(part.ob))(part,data,emap)
    )
  

def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],ob: Ob,src: PartProp,tgt: PartProp): EntitySource[D,A] =
  ACSetEdgeSource[D,A](sprite,ob -> (src,tgt))

def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],es:(Ob,(PartProp,PartProp))*): EntitySource[D,A] =
  ACSetEdgeSource[D,A](sprite,es.toMap)

def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],edef:(Ob => Option[(PartProp,PartProp)])): EntitySource[D,A] =
  ACSetEdgeSource[D,A](sprite,edef.unlift)


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

def wireProps[D:PartData](
  e:Part,
  src: PartProp,
  tgt: PartProp,
  // typeProps: (D, Part) => PropMap,
  wireDir: Part => Complex = _ => Complex(0, 0),
  // bg: => Part = ROOT
)(_e: Part, data: D, m: EntityMap[D]): PropMap = 

  val defaultPos = Complex(50, 50)

  val p = data.getProps()

  val propOpt = for
    s <- p.get(src)
    t <- p.get(tgt)
    sc <- findCenter(s,m)
    tc <- findCenter(t,m)
    (sspr,sdata) <- m.get(s)
    (tspr,tdata) <- m.get(t)
    sd = wireDir(s)
    td = wireDir(t)
  yield (
      PropMap()
        .set(Start, sc)
        .set(End, tc)
        .set(StartDir, sd)
        .set(EndDir, td)
        .set(TikzStart, s.tikzName)
        .set(TikzEnd, t.tikzName)  
    )
    
  propOpt.getOrElse(PropMap())
  
  




