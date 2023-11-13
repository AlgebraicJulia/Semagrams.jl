// package semagrams.sprites

// import semagrams._
// import semagrams.acsets.abstr._
// import semagrams.util._
// import semagrams.acsets.simple.BasicSchema

// /** An [[SpriteSource]] that extracts all parts of type `ob` from an ACSet and
//   * pairs them with `sprite` along with their subacset
//   */
// def ACSetEntitySource[D:PartData,A:ACSetWithData[D]](
//     sprite: Sprite[D],
//     test: Ob => Boolean
// ): SpriteSource[D,A] =
//   SpriteSource[D,A]((acs, _m) =>
//     acs.schema.obs.filter(test)
//       .flatMap(ob => acs.getData(ob).toSeq)
//       .map( (part, data) =>
//         (part, sprite, data)
//       )
//   )

// def ACSetEntitySource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],obs: Ob*): SpriteSource[D,A] =
//   ACSetEntitySource[D,A](sprite,obs.contains)




// /** Like [[ACSetEntitySource]], but then also computes the edge properties using
//   * [[edgeProps]]
//   */
// def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](
//     sprite: Sprite[D],
//     edef: PartialFunction[Ob,(PartProp,PartProp)]
// ): SpriteSource[D,A] = 
//   ACSetEntitySource[D,A](sprite,edef.isDefinedAt)
//     .addPropsBy( (part,data,emap) =>
//       edgeProps[D].tupled(edef(part.ob))(part,data,emap)
//     )
  

// def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],ob: Ob,src: PartProp,tgt: PartProp): SpriteSource[D,A] =
//   ACSetEdgeSource[D,A](sprite,ob -> (src,tgt))

// def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],es:(Ob,(PartProp,PartProp))*): SpriteSource[D,A] =
//   ACSetEdgeSource[D,A](sprite,es.toMap)

// def ACSetEdgeSource[D:PartData,A:ACSetWithData[D]](sprite: Sprite[D],edef:(Ob => Option[(PartProp,PartProp)])): SpriteSource[D,A] =
//   ACSetEdgeSource[D,A](sprite,edef.unlift)


// /** Similar to [[edgeProps]]. Computes the position and direction for the ends
//   * of a wire from the ports it is connected to, using the top-level properties
//   * in `acs` and the other sprites in `m`.
//   *
//   * If `src`/`tgt` are present, it uses those, otherwise it looks up an explicit
//   * `Start`/`End` value in `acs`
//   *
//   * Takes an optional callback argument `bg: => Part` to relativize the lookup
//   * to a variable background for zooming in and out.
//   */

// def wireProps[D:PartData](
//   e:Part,
//   src: PartProp,
//   tgt: PartProp,
//   // typeProps: (D, Part) => PropMap,
//   wireDir: Part => Complex = _ => Complex(0, 0),
//   // bg: => Part = ROOT
// )(_e: Part, data: D, m: SpriteMap[D]): PropMap = 

//   val defaultPos = Complex(50, 50)

//   val p = data.getProps()

//   val propOpt = for
//     s <- p.get(src)
//     t <- p.get(tgt)
//     sc <- findCenter(s,m)
//     tc <- findCenter(t,m)
//     (sspr,sdata) <- m.get(s)
//     (tspr,tdata) <- m.get(t)
//     sd = wireDir(s)
//     td = wireDir(t)
//   yield (
//       PropMap()
//         .set(Start, sc)
//         .set(End, tc)
//         .set(StartDir, sd)
//         .set(EndDir, td)
//         .set(TikzStart, s.tikzName)
//         .set(TikzEnd, t.tikzName)  
//     )
    
//   propOpt.getOrElse(PropMap())
  
  




