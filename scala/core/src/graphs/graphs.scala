package semagrams.graphs

import upickle.default._

import semagrams._
import semagrams.acsets.abstr._
import semagrams.sprites._
import semagrams.layout.assignBends
// // import semagrams.sprites._
// // import semagrams.layout.assignBends
// // import semagrams.acsets.abstr._



// import semagrams.acsets.abstr.{Ob,Generator}

enum GraphOb(val name:String) extends GenOb
  derives ReadWriter:
  case V extends GraphOb("V")
  case E extends GraphOb("E")
export GraphOb._


enum GraphHom(val name:String,val dom:GraphOb,val codom:GraphOb) 
  extends GenHom[GraphOb]:
  case Src extends GraphHom("src",E,V)
  case Tgt extends GraphHom("tgt",E,V)
export GraphHom._


case object SchGraph
val schGraphIsSchema: Schema[SchGraph.type] = new Schema[SchGraph.type] {
  val name = "schGraphIsSchema"
  type SchOb = GraphOb
  type SchHom = GraphHom

  extension (s:SchGraph.type)
    def obs = GraphOb.values.toSeq
    def homs = GraphHom.values.toSeq
    def attrs = Seq()



}

// object Graph {
//   def apply() = SimpleACSet(SchGraph)
// }

  



sealed trait GraphEltDef[D:PartData]:
  val ob: Ob
  val sprite: Sprite[D]
  val init: D
// object GraphEltDef:

//   // type GraphEltSpec[D] = Ob | //(AOb,Sprite[D]) |
//   //   (AOb,D) | (AOb,PropMap) | 
//   //   (AOb,(Sprite[D],D)) | (AOb,(Sprite[D],PropMap)) |
//   //   (AOb,(AHom,AHom)) | (AOb,(AHom,AHom,Sprite[D])) |
//   //   (AOb,(AHom,AHom,D)) | (AOb,(AHom,AHom,PropMap)) | 
//   //   (AOb,(AHom,AHom,Sprite[D],D)) | (AOb,(AHom,AHom,Sprite[D],PropMap))

//   // def data[D:PartData](p:PropMap) = implicitly[PartData[D]].toData(p)
//   // def data[D:PartData](): D = data[D](PropMap())

//   def apply[X<:Ob,D:PartData](ob:X): GraphEltDef[X,D] = 
//     VertexDef(ob,ShapeNode[D](),PartData[D]())
  
//   def apply[X<:Ob,D:PartData](ob:X,sprite:Sprite[D]): GraphEltDef[X,D] = 
//     val init: PropMap = PropMap() + (Bend,2.0)
//     VertexDef(ob,sprite,PartData[D](init))
  
//   // def apply[D:PartData](ob:Ob,init:PropMap): GraphEltDef[D] = 
//   //   val d = PartData[D](init)
//   //   VertextDef(ob,ShapeNode[D](),d)




//     // VertexDef(ob,ShapeNode[D](),PartData[D](init))
//     // VertexDef(ob,ShapeNode(),PartData[D]())
//     // eltSpec match
//   //   /* Vertex defs */
//   //   case ob:AOb => 
//   //     VertexDef(ob,ShapeNode(),data())
//   //   case (ob:AOb,spec) => spec match
//   //     case spr:Sprite[D] => 
//   //       VertexDef(ob,spr,data())
//   //     case init:D => 
//   //       VertexDef(ob,ShapeNode(),init)
//   //     case init:PropMap => 
//   //       VertexDef(ob,ShapeNode(),data(init))
//   //     case (spr:Sprite[D],init:D) => 
//   //       VertexDef(ob,spr,init)
//   //     case (spr:Sprite[D],init:PropMap) => 
//   //       VertexDef(ob,spr,data(init))
//   //   /* Edge defs */
//   //     case (src:AHom,tgt:AHom) =>
//   //       EdgeDef(ob,src,tgt,Arrow(),data())
//   //     case (src:AHom,tgt:AHom,spr:Sprite[D]) =>
//   //       EdgeDef(ob,src,tgt,spr,data())        
//   //     case (src:AHom,tgt:AHom,init:D) =>
//   //       EdgeDef(ob,src,tgt,Arrow(),init)
//   //     case (src:AHom,tgt:AHom,init:PropMap) =>
//   //       EdgeDef(ob,src,tgt,Arrow(),data(init))
//   //     case (src:AHom,tgt:AHom,spr:Sprite[D],init:D) =>
//   //       EdgeDef(ob,src,tgt,spr,init)
//   //     case (src:AHom,tgt:AHom,spr:Sprite[D],init:PropMap) =>
//   //       EdgeDef(ob,src,tgt,spr,data(init))



case class VertexDef[D:PartData](
  ob:Ob, 
  sprite:Sprite[D], 
  init: D
) extends GraphEltDef[D]

case class EdgeDef[D:PartData](
  ob:Ob, 
  src:PartProp, 
  tgt:PartProp, 
  sprite:Sprite[D],
  init: D
) extends GraphEltDef[D]




// implicit val acsetIsACSet[S:Schema](s:S): ACSetWithSchema[SimpleACSet[S]]
//   = simpleACSetIsACSet(s)
//   PartData[PropMap] = PartData.propsAreData
// def acsetIsACSet[D]: ACSetWithDataAndS

case class GraphDisplay[
  S:Schema,
  D:PartData,
  A
](
  schema: S,
  vertexDefs: Seq[VertexDef[D]],
  edgeDefs: Seq[EdgeDef[D]]
)(using acsetDef: ACSetWithSchAndData2[S,D][A]) extends ACSemagram[S,D,A]:


  // type Data = D
  // implicit val dataIsPartData: PartData[D] = 
  //   summon[PartData[D]]
  // def layout(m: GraphDisplay.this.Model): GraphDisplay.this.Model = ???
  // implicit val dataIsPartData = dIsData


  val eltDefs: Seq[GraphEltDef[D]] = vertexDefs ++ edgeDefs

  def layout(g: A): A =
    val partProps = eltDefs.flatMap(defn => g.getProps(defn.ob))
    
    val gWithProps = g.softSetProps(partProps)

    assignBends(
      edgeDefs.map(e => e.ob -> (e.src,e.tgt)).toMap,
      0.5
    )(gWithProps)

  val entitySources =
    val vSources = vertexDefs.map(vdef => 
      ACSetEntitySource[D,A](vdef.ob,vdef.sprite)(
        summon[PartData[D]],
        acsetDef
      )
    ) 
    val eSources = edgeDefs.map(edef => 
      ACSetEdgeSource[D,A](edef.ob,edef.src,edef.tgt,edef.sprite)
    )
    vSources ++ eSources

  
    

// // object GraphDisplay:
// //   import GraphEltDef._

//   // def apply[A<:AbstractACSet[A]{ type D = PropMap}](schema:AbstractSchema,specs:GraphEltSpec[PropMap]*): GraphDisplay[A] =
//   //   val defs = specs.map(GraphEltDef.apply)
//   //   new GraphDisplay[A](
//   //     schema,
//   //     defs.collect{ case v:VertexDef[PropMap] => v},
//   //     defs.collect{ case e:EdgeDef[PropMap] => e},
//   //   )

//     // def apply[A<:AbstractACSet[_A]{ type D = PropMap }]
//     //   (eltDefs:Seq[GraphEltDef]): GraphDisplay[A] = new GraphDisplay(
//     //   eltDefs.collect{ case v:VertexDef => v},
//     //   eltDefs.collect{ case e:EdgeDef => e}
//     // )
//     // import SchGraph._

//     // def apply[
//     //   D : PartData,
//     //   A<:AbstractACSet[A]{ type D = D }
//     // ](
//     //   schema: AbstractSchema,
//     //   eltDefs:Seq[
//     //       /* Vertex defs */
//     //       schema.Ob | 
//     //       (schema.Ob,Sprite[PropMap]) | 
//     //       (schema.Ob,PropMap) | (schema.Ob,PropMap) |
//     //       (schema.Ob,(Sprite[PropMap],PropMap)) |
//     //       (schema.Ob,(Sprite[PropMap],PropMap)) |
//     //       /* Edge defs */
//     //       (schema.Ob,(schema.Hom,schema.Hom)) | 
//     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap])) |
//     //       (schema.Ob,(schema.Hom,schema.Hom,PropMap)) |
//     //       (schema.Ob,(schema.Hom,schema.Hom,PropMap)) | 
//     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap], PropMap)) |
//     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap], PropMap))
//     //     ]
//     //   ): GraphDisplay[A] =

//     //   // def mkData(): 
//     //   implicit val pd: PartData[D] = summon[PartData[D]]
      
//     //   val defs = eltDefs.map{
//     //     /* Vertex cases */
//     //     case ob:schema.Ob => 
//     //       VertexDef(ob,ShapeNode(),PropMap())
//     //     case (ob:schema.Ob,sprite:Sprite[D]) => 
//     //       VertexDef(ob,sprite,PropMap())(pd)
//     //     case (ob:schema.Ob,init:PropMap) => 
//     //       VertexDef(ob,ShapeNode(), init)
//     //     case (ob:schema.Ob,(sprite:Sprite[_],init:PropMap)) => 
//     //       VertexDef(ob,sprite, init)
        
//     //     /* Edge defs */
//     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom)) =>
//     //       EdgeDef(ob,src,tgt,Arrow(),PropMap())
//     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,sprite:Sprite[PropMap])) =>
//     //       EdgeDef(ob,src,tgt,sprite,PropMap())
//     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,init:PropMap)) =>
//     //       EdgeDef(ob,src,tgt,Arrow(),init)
//     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,sprite:Sprite[PropMap],init:PropMap)) =>
//     //       EdgeDef(ob,src,tgt,sprite,mkData(d))
//     //   }






