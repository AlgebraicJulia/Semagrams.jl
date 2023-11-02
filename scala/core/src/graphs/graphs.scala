package semagrams.graphs

import upickle.default._

import semagrams._
import semagrams.acsets.abstr._
import semagrams.layout.assignBends
import semagrams.bindings.Binding
import cats.effect.IO
import scala.annotation.targetName
import semagrams.acsets.simple.SchObs



// import semagrams.acsets.abstr.{Ob,Generator}

enum GraphOb(val name:String) extends GenOb
  derives ReadWriter:
  case V extends GraphOb("V")
  case E extends GraphOb("E")
  val id = util.UUID("GraphOb")
export GraphOb._


enum GraphHom(val name:String,val dom:GraphOb,val codom:GraphOb) 
  extends GenHom[GraphOb]:
  case Src extends GraphHom("src",E,V)
  case Tgt extends GraphHom("tgt",E,V)
  val id = util.UUID("GraphHom")
export GraphHom._


case object SchGraph
val schGraphIsSchema: Schema[SchGraph.type] = new Schema[SchGraph.type] {
  val name = "schGraphIsSchema"
  val emptySchema = SchGraph
  type SchOb = GraphOb
  type SchHom = GraphHom

  extension (s:SchGraph.type)
    def obMap = genMap(GraphOb.values)
    def homMap = genMap(GraphHom.values)
    def attrMap = Map()

    def _addElts(elts:Seq[Generator]) = 
      println("SchGraph is static")
      s

    def addProps(newProps:Seq[Property]) = 
      println("SchGraph is static")
      s

    def _remElts(elts:Seq[Generator]) = 
      println("SchGraph is static")
      s

    def remProps(oldProps:Seq[Property]) = 
      println("SchGraph is static")
      s



}

// object Graph {
//   def apply() = SimpleACSet(SchGraph)
// }




sealed trait GraphEltDef[D:PartData]:
  val sprite: Sprite[D]
  val init: PartialFunction[Ob,D]

  val test = init.isDefinedAt

  def asEntitySource[A:ACSetWithData[D]]: EntitySource[D,A]

  // def transform(f:D => D): GraphEltDef[D]
  // def stateTransform(es:EditorState)(part:Part,data:D): D

object GraphEltDef:
  def makeEntitySource[D:PartData,A:ACSetWithData[D]](defn:GraphEltDef[D]): EntitySource[D,A] = 
    EntitySource[D,A]( (acset,emap) => for
      ob <- acset.schema.obs
      if defn.test(ob)
      // _ = if ob == SchObs.FKeyOb
      //   then 
      //     println(s"makeEntitySource $ob")
      //     println(s"parts = ${acset.getData(ob)}")
      (part,data) <- acset.getData(ob)
      newData = defn.init(ob).merge(data)
    yield (part,defn.sprite,newData))
    
    // baseES.softAddDataBy( (part,_,_) => 
    //   defn.init.applyOrElse(part.ob,(_ => PartData[D]()))
    // )

case class VertexDef[D:PartData](
  sprite:Sprite[D],
  init: PartialFunction[Ob,D]
) extends GraphEltDef[D]:

  def asEntitySource[A:ACSetWithData[D]]: EntitySource[D,A] = 
    GraphEltDef.makeEntitySource(this)

  // def transform(f:D => D): VertexDef[D] = VertexDef(sprite,init.andThen(f))
  




object VertexDef:
  def apply[D:PartData](sprite: Sprite[D],ob:Ob,obs: Ob*): VertexDef[D] =
    new VertexDef(sprite,(ob +: obs).map(ob => ob -> PartData[D]()).toMap)
  def apply[D:PartData](sprite: Sprite[D],d:D,ob:Ob,obs: Ob*): VertexDef[D] =
    new VertexDef(sprite,(ob +: obs).map(ob => ob -> d).toMap)
  def apply[D:PartData](sprite: Sprite[D],ob:Ob,d:D): VertexDef[D] =
    new VertexDef(sprite,Map(ob -> d))
  def apply[D:PartData](sprite: Sprite[D],init:(Ob,D),inits:(Ob,D)*): VertexDef[D] =
    new VertexDef(sprite,(init +: inits).toMap)
  def apply[D:PartData](sprite: Sprite[D],lifted:Ob => Option[D]): VertexDef[D] =
    new VertexDef(sprite,lifted.unlift)
  def apply[D:PartData](sprite: Sprite[D],pf:PartialFunction[Ob,D]): VertexDef[D] =
    new VertexDef(sprite,pf)
    
type EDef[D] = (D,PartProp,PartProp)

case class EdgeDef[D:PartData](
  sprite:Sprite[D],
  edgeData: PartialFunction[Ob,EDef[D]]
) extends GraphEltDef[D]:
  
  val init = edgeData.andThen(_._1)

  // def transform(f:D => D): EdgeDef[D] = EdgeDef(sprite,
  //   edgeData.andThen( (d,src,tgt) => (f(d),src,tgt))
  // )



  def asEntitySource[A:ACSetWithData[D]]: EntitySource[D,A] = 
    GraphEltDef.makeEntitySource(this)
      .addPropsBy( (part,data,emap) =>
        val (_,src,tgt) = edgeData(part.ob) 
        sprites.edgeProps[D](src,tgt)(part,data,emap)
      )

  



object EdgeDef:
  def apply[D:PartData](sprite:Sprite[D],props:PropMap,ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
    EdgeDef(sprite, PartData[D](props), ob -> (src,tgt))
//   def apply[D:PartData](sprite:Sprite[D],d:D,ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
//     EdgeDef(sprite, IO(d), ob -> (src,tgt))
//   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
//     EdgeDef(sprite, dIO, ob -> (src,tgt))
// EdgeDef(Arrow(Content),PropMap() + (Stroke,"purple"),FKeyOb,FKeySrc,FKeyTgt)

  def apply[D:PartData](sprite:Sprite[D],d:D,es:(Ob,(PartProp,PartProp))*): EdgeDef[D] =
    new EdgeDef(sprite, es.map{ case (ob,(s,t)) => ob -> (d,s,t) }.toMap )
//   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],es:(Ob,(PartProp,PartProp))*): EdgeDef[D] =
//     EdgeDef(sprite, dIO, es.toMap)

//   // def apply[D:PartData](sprite:Sprite[D],props:PropMap,eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
//   //   EdgeDef(sprite, IO(PartData[D](props)), eOpt.unlift)
//   def apply[D:PartData](sprite:Sprite[D],d:D,eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
//     EdgeDef(sprite, IO(d), eOpt.unlift)
//   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
//     EdgeDef(sprite, dIO, eOpt.unlift)

//   // def apply[D:PartData](sprite:Sprite[D],props:PropMap,edef:PartialFunction[Ob,(PartProp,PartProp)]): EdgeDef[D] =
//   //   EdgeDef(sprite, IO(PartData[D](props)), edef)
//   def apply[D:PartData](sprite:Sprite[D],d:D,edef:PartialFunction[Ob,(PartProp,PartProp)]): EdgeDef[D] =
//     EdgeDef(sprite, IO(d), edef)



  




// implicit val acsetIsACSet[S:Schema](s:S): ACSetWithSchema[SimpleACSet[S]]
//   = simpleACSetIsACSet(s)
//   PartData[PropMap] = PartData.propsAreData
// def acsetIsACSet[D]: ACSetWithDataAndS

case class GraphDisplay[D:PartData,A:ACSetWithData[D]](
  vertexDefs: Seq[VertexDef[D]],
  edgeDefs: Seq[EdgeDef[D]],
  transform: (EditorState,A) => Message[A] = (es:EditorState,a:A) => Message[A]()
):
  
  def apply(
    bindings:Seq[Binding[A]],
    init:A
  ): SemagramElt[D,A] = {
    /* Create ACSemagram */
    
    val newSema = new ACSemagram[D,A] {

      override def stateMsg(es: EditorState,a:A): Message[A] = transform(es,a)

      val eltDefs: Seq[GraphEltDef[D]] = vertexDefs ++ edgeDefs


      def layout(g: A): A = {
        val es = for 
          edef <- edgeDefs
          ob <- g.schema.obs
          if edef.edgeData.isDefinedAt(ob)
          (d,s,t) = edef.edgeData(ob)
        yield ob -> (s,t)
      
        assignBends(es.toMap,0.5)(g)
      }

      
      val entitySources = eltDefs.map(_.asEntitySource[A])

    }
    /* Return SemagramElt */
    newSema.apply(bindings,init)




}


type GraphDefSeq[D] = GraphEltDef[D] | Seq[GraphEltDef[D]]
// object GraphDisplay:
//   def apply[D:PartData](defs:GraphDefSeq[D]*) =
//     val clean = defs.flatMap(ds => ds match
//         case defs:Seq[GraphEltDef[D]] => defs
//         case defn:GraphEltDef[D] => Seq(defn)
//       )
//     new GraphDisplay(
//       clean.collect{ case vdef:VertexDef[D] => vdef},
//       clean.collect{ case edef:EdgeDef[D] => edef}
//     )


  // type Data = D
  // implicit val dataIsPartData: PartData[D] = 
  //   summon[PartData[D]]
  // def layout(m: GraphDisplay.this.Model): GraphDisplay.this.Model = ???
  // implicit val dataIsPartData = dIsData



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




