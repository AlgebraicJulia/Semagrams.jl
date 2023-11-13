// package semagrams.acsets.nested

// import semagrams.layout.assignBends
// import semagrams.ACSemagram
// import semagrams.sprites._
// // import semagrams.sprites.{ACSetEntitySource,ACSetEdgeSource}
// import semagrams.Sprite
// import semagrams.PropMap

// object Graphs {
//   case object V extends NestedOb
//   case object E extends NestedOb

//   case object Src extends Hom {
//     val doms = E.asDom()
//     val codoms = V.asDom()
//   }
//   case object Tgt extends Hom {
//     val doms = E.asDom()
//     val codoms = V.asDom()
//   }

//   case object SchGraph extends Schema {
//     val obs = Seq(V, E)
//     val homs = Seq(Src, Tgt)
//     val attrs = Seq()
//   }

//   object Graph {
//     def apply() = ACSet(SchGraph)
//   }


//   sealed trait GraphEltDef
//   case class VertexDef(ob:NestedOb,sprite:Sprite,props:PropMap) extends GraphEltDef
//   case class EdgeDef(ob:NestedOb,src:Hom,tgt:Hom,sprite:Sprite,props:PropMap) extends GraphEltDef

//   case class GraphDisplay(
//     schema: Schema,
//     vertexDefs: Seq[VertexDef],
//     edgeDefs: Seq[EdgeDef]
//   ) extends ACSemagram:
//     def layout(g: ACSet) = 
//       val gWithProps = g.softSetGlobalProps(
//         vertexDefs.map(v => v.ob -> v.props) ++
//         edgeDefs.map(e => e.ob -> e.props)
//       )

//       assignBends(
//         edgeDefs.map(e => e.ob -> (e.src,e.tgt)).toMap,
//         0.5
//       )(gWithProps)

//     val spriteSources = 
//       vertexDefs.map(v => ACSetEntitySource(v.ob,v.sprite)) ++
//       edgeDefs.map(e => ACSetEdgeSource(e.ob,e.src,e.tgt,e.sprite))

    
      

//   object GraphDisplay:
//     // def apply(eltDefs:Seq[GraphEltDef]): GraphDisplay = new GraphDisplay(
//     //   eltDefs.collect{ case v:VertexDef => v},
//     //   eltDefs.collect{ case e:EdgeDef => e}
//     // )

//     def apply(
//       schema: Schema,
//       eltDefs:Seq[
//           /* Vertex defs */
//           NestedOb | (NestedOb,PropMap) |
//           (NestedOb,Sprite) | (NestedOb,(Sprite,PropMap)) |
//           /* Edge defs */
//           (NestedOb,(Hom,Hom)) | (NestedOb,(Hom,Hom,Sprite)) |
//           (NestedOb,(Hom,Hom,PropMap)) | (NestedOb,(Hom,Hom,Sprite,PropMap))
//         ]
//       ): GraphDisplay = 
//       val defs = eltDefs.map{
//         /* Vertex cases */
//         case ob:NestedOb => 
//           VertexDef(ob,ShapeNode(),PropMap())
//         case (ob:NestedOb,props:PropMap) => 
//           VertexDef(ob,ShapeNode(),props)
//         case (ob:NestedOb,sprite:Sprite) => 
//           VertexDef(ob,sprite,PropMap())
//         case (ob:NestedOb,(sprite:Sprite,props:PropMap)) => 
//           VertexDef(ob,sprite,props)
        
//         /* Edge defs */
//         case (ob:NestedOb,(src:Hom,tgt:Hom)) =>
//           EdgeDef(ob,src,tgt,Arrow(),PropMap())
//         case (ob:NestedOb,(src:Hom,tgt:Hom,sprite:Sprite)) =>
//           EdgeDef(ob,src,tgt,sprite,PropMap())
//         case (ob:NestedOb,(src:Hom,tgt:Hom,props:PropMap)) =>
//           EdgeDef(ob,src,tgt,Arrow(),props)
//         case (ob:NestedOb,(src:Hom,tgt:Hom,sprite:Sprite,props:PropMap)) =>
//           EdgeDef(ob,src,tgt,sprite,props)
//       }
//       GraphDisplay(
//         schema,
//         defs.collect{ case v:VertexDef => v},
//         defs.collect{ case e:EdgeDef => e},
//       )





// }
