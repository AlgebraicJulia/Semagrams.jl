package semagrams.acsets

import semagrams.layout.assignBends
import semagrams.ACSemagram
import semagrams.sprites._
// import semagrams.sprites.{ACSetEntitySource,ACSetEdgeSource}
import semagrams.Sprite
import semagrams.PropMap

object Graphs {
  case object V extends Ob
  case object E extends Ob

  case object Src extends Hom {
    val doms = E.asDom()
    val codoms = V.asDom()
  }
  case object Tgt extends Hom {
    val doms = E.asDom()
    val codoms = V.asDom()
  }

  case object SchGraph extends Schema {
    val obs = Seq(V, E)
    val homs = Seq(Src, Tgt)
    val attrs = Seq()
  }

  object Graph {
    def apply() = ACSet(SchGraph)
  }


  sealed trait GraphEltDef
  case class VertexDef(ob:Ob,sprite:Sprite,props:PropMap) extends GraphEltDef
  case class EdgeDef(ob:Ob,src:Hom,tgt:Hom,sprite:Sprite,props:PropMap) extends GraphEltDef

  case class GraphDisplay(
    schema: Schema,
    vertexDefs: Seq[VertexDef],
    edgeDefs: Seq[EdgeDef]
  ) extends ACSemagram:
    def layout(g: ACSet) = 
      val gWithProps = g.softSetGlobalProps(
        vertexDefs.map(v => v.ob -> v.props) ++
        edgeDefs.map(e => e.ob -> e.props)
      )

      assignBends(
        edgeDefs.map(e => e.ob -> (e.src,e.tgt)).toMap,
        0.5
      )(gWithProps)

    val entitySources = 
      vertexDefs.map(v => ACSetEntitySource(v.ob,v.sprite)) ++
      edgeDefs.map(e => ACSetEdgeSource(e.ob,e.src,e.tgt,e.sprite))

    
      

  object GraphDisplay:
    // def apply(eltDefs:Seq[GraphEltDef]): GraphDisplay = new GraphDisplay(
    //   eltDefs.collect{ case v:VertexDef => v},
    //   eltDefs.collect{ case e:EdgeDef => e}
    // )

    def apply(
      schema: Schema,
      eltDefs:Seq[
          /* Vertex defs */
          Ob | (Ob,PropMap) |
          (Ob,Sprite) | (Ob,(Sprite,PropMap)) |
          /* Edge defs */
          (Ob,(Hom,Hom)) | (Ob,(Hom,Hom,Sprite)) |
          (Ob,(Hom,Hom,PropMap)) | (Ob,(Hom,Hom,Sprite,PropMap))
        ]
      ): GraphDisplay = 
      val defs = eltDefs.map{
        /* Vertex cases */
        case ob:Ob => 
          VertexDef(ob,ShapeNode(),PropMap())
        case (ob:Ob,props:PropMap) => 
          VertexDef(ob,ShapeNode(),props)
        case (ob:Ob,sprite:Sprite) => 
          VertexDef(ob,sprite,PropMap())
        case (ob:Ob,(sprite:Sprite,props:PropMap)) => 
          VertexDef(ob,sprite,props)
        
        /* Edge defs */
        case (ob:Ob,(src:Hom,tgt:Hom)) =>
          EdgeDef(ob,src,tgt,Arrow(),PropMap())
        case (ob:Ob,(src:Hom,tgt:Hom,sprite:Sprite)) =>
          EdgeDef(ob,src,tgt,sprite,PropMap())
        case (ob:Ob,(src:Hom,tgt:Hom,props:PropMap)) =>
          EdgeDef(ob,src,tgt,Arrow(),props)
        case (ob:Ob,(src:Hom,tgt:Hom,sprite:Sprite,props:PropMap)) =>
          EdgeDef(ob,src,tgt,sprite,props)
      }
      GraphDisplay(
        schema,
        defs.collect{ case v:VertexDef => v},
        defs.collect{ case e:EdgeDef => e},
      )





}
