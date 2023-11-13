package semagrams.graphs

import upickle.default._
import com.raquo.laminar.api.L._

import semagrams._
import semagrams.acsets._
import semagrams.rendering.{SpriteMap,SpriteSource}




enum GraphOb(val _name:String,val id:UUID) extends Ob with Generator
  derives ReadWriter:
  case V extends GraphOb("V",UUID("V"))
  case E extends GraphOb("E",UUID("E"))
  name = _name
  def generators = this.obGenerators
export GraphOb._


enum GraphHom(val _name:String,val _id:UUID,val dom:GraphOb,val codom:GraphOb) 
  extends Hom[GraphOb,GraphOb] with Generator:
  case Src extends GraphHom("Src",UUID("Src"),E,V)
  case Tgt extends GraphHom("Tgt",UUID("Tgt"),E,V)

  type Value = Part
  val rw = Part.rw
  name = _name
  val id = _id
  def generators = this.homGenerators
  def path = this.path
export GraphHom._


case object SchGraph extends Schema:

  val id = UUID("SchGraph")

  var name = "SchGraph"

  def elts = GraphOb.values.eltMap ++ GraphHom.values.eltMap
  def globalProps = Seq()





object Graph {
  def apply() = ACSet[PropMap](SchGraph)
}




sealed trait GraphEltDef[D:PartData]:
  val sprite: Sprite[D]
  val init: PartialFunction[Ob,D]

  val test = init.isDefinedAt

  def spriteSource: SpriteSource[ACSet[D],D]

  // def transform(f:D => D): GraphEltDef[D]
  // def stateTransform(es:EditorState)(part:Part,data:D): D

object GraphEltDef:
  def makeSpriteSource[D:PartData](defn:GraphEltDef[D]): SpriteSource[ACSet[D],D] =
    SpriteSource[ACSet[D],D]( (acset,emap) => 
      val triples = acset.schema.obSeq
        .filter(defn.test)
        .flatMap(ob => 
          acset.getDataSeq(ob).map((part,data) =>
            part -> (defn.sprite,defn.init(ob).merge(data))  
          )        
        )
      triples.reverse
    )

    
    // baseES.softAddDataBy( (part,_,_) => 
    //   defn.init.applyOrElse(part.ob,(_ => PartData[D]()))
    // )

case class VertexDef[D:PartData](
  sprite:Sprite[D],
  init: PartialFunction[Ob,D]
) extends GraphEltDef[D]:

  def spriteSource: SpriteSource[ACSet[D],D] = 
    GraphEltDef.makeSpriteSource(this)

  // def transform(f:D => D): VertexDef[D] = VertexDef(sprite,init.andThen(f))
  




object VertexDef:
//   def apply[D:PartData](sprite: Sprite[D],ob:Ob,obs: Ob*): VertexDef[D] =
//     new VertexDef(sprite,(ob +: obs).map(ob => ob -> PartData[D]()).toMap)
  def apply[D:PartData](sprite: Sprite[D],d:D,ob:Ob,obs: Ob*): VertexDef[D] =
    new VertexDef(sprite,(ob +: obs).map(ob => ob -> d).toMap)
//   def apply[D:PartData](sprite: Sprite[D],ob:Ob,d:D): VertexDef[D] =
//     new VertexDef(sprite,Map(ob -> d))
//   def apply[D:PartData](sprite: Sprite[D],init:(Ob,D),inits:(Ob,D)*): VertexDef[D] =
//     new VertexDef(sprite,(init +: inits).toMap)
//   def apply[D:PartData](sprite: Sprite[D],lifted:Ob => Option[D]): VertexDef[D] =
//     new VertexDef(sprite,lifted.unlift)
//   def apply[D:PartData](sprite: Sprite[D],pf:PartialFunction[Ob,D]): VertexDef[D] =
//     new VertexDef(sprite,pf)
    
type EData[D] = (D,PartProp,PartProp)

case class EdgeDef[D:PartData](
  sprite:Sprite[D],
  edgeData: PartialFunction[Ob,EData[D]]
) extends GraphEltDef[D]:
  
  val init = edgeData.andThen(_._1)

  // def transform(f:D => D): EdgeDef[D] = EdgeDef(sprite,
  //   edgeData.andThen( (d,src,tgt) => (f(d),src,tgt))
  // )



  def spriteSource: SpriteSource[ACSet[D],D] = 
    GraphEltDef.makeSpriteSource(this)
      .addPropsBy( (part,spritemap,data) =>
        val (_,src,tgt) = edgeData(part.ob) 
        edgeProps[D](src,tgt)(part,data,spritemap)
      )

object EdgeDef:
  def apply[D:PartData](sprite:Sprite[D],edgeOb:Ob,src:PartHom,tgt:PartHom) =
    assert(src.dom == edgeOb & edgeOb == tgt.dom)
    new EdgeDef(sprite,{
      case ob:Ob if ob == edgeOb => (PartData(),src,tgt)
    })


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
)(_e: Part, data: D, m: SpriteMap[D]): PropMap = {
  val p = data.getProps()
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s.flatMap(m.findCenter).getOrElse(
    p.get(Start).getOrElse(Complex(100,100))
  )
  val tpos = t.flatMap(m.findCenter).getOrElse(
    p.get(End).getOrElse(Complex(100,100))
  )
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = Complex(0, -bend).exp
  val start = s
    .flatMap(m.findBoundary(_, - dir * rot))
    .getOrElse(spos)
  val theend = t
    .flatMap(m.findBoundary(_, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, t) match
    case (Some(p), Some(q)) =>
      PropMap().set(TikzStart, p.tikzName).set(TikzEnd, q.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, theend)
}


// object EdgeDef:
//   def apply[D:PartData](sprite:Sprite[D],props:PropMap,ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
//     EdgeDef(sprite, PartData[D](props), ob -> (src,tgt))
// //   def apply[D:PartData](sprite:Sprite[D],d:D,ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
// //     EdgeDef(sprite, IO(d), ob -> (src,tgt))
// //   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],ob:Ob,src:PartProp,tgt:PartProp): EdgeDef[D] =
// //     EdgeDef(sprite, dIO, ob -> (src,tgt))
// // EdgeDef(Arrow(Content),PropMap() + (Stroke,"purple"),FKeyOb,FKeySrc,FKeyTgt)

//   def apply[D:PartData](sprite:Sprite[D],d:D,es:(Ob,(PartProp,PartProp))*): EdgeDef[D] =
//     new EdgeDef(sprite, es.map{ case (ob,(s,t)) => ob -> (d,s,t) }.toMap )
// //   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],es:(Ob,(PartProp,PartProp))*): EdgeDef[D] =
// //     EdgeDef(sprite, dIO, es.toMap)

// //   // def apply[D:PartData](sprite:Sprite[D],props:PropMap,eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
// //   //   EdgeDef(sprite, IO(PartData[D](props)), eOpt.unlift)
// //   def apply[D:PartData](sprite:Sprite[D],d:D,eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
// //     EdgeDef(sprite, IO(d), eOpt.unlift)
// //   def apply[D:PartData](sprite:Sprite[D],dIO:IO[D],eOpt:Ob => Option[(PartProp,PartProp)]): EdgeDef[D] =
// //     EdgeDef(sprite, dIO, eOpt.unlift)

// //   // def apply[D:PartData](sprite:Sprite[D],props:PropMap,edef:PartialFunction[Ob,(PartProp,PartProp)]): EdgeDef[D] =
// //   //   EdgeDef(sprite, IO(PartData[D](props)), edef)
// //   def apply[D:PartData](sprite:Sprite[D],d:D,edef:PartialFunction[Ob,(PartProp,PartProp)]): EdgeDef[D] =
// //     EdgeDef(sprite, IO(d), edef)



  




// implicit val acsetIsACSet[S:Schema](s:S): ACSetWithSchema[SimpleACSet[S]]
//   = simpleACSetIsACSet(s)
//   PartData[PropMap] = PartData.propsAreData
// def acsetIsACSet[D]: ACSetWithDataAndS

case class GraphDisplay[D:PartData](
  vertexDefs: Seq[VertexDef[D]],
  edgeDefs: Seq[EdgeDef[D]],
  _layout: (EditorState,ACSet[D]) => ACSet[D] = (_,a:ACSet[D]) => a
)(
  override val bindings: Seq[Binding[ACSet[D]]],
  override val modelVar: UndoableVar[ACSet[D]]
) extends ACSemagram[D]:

  val eltDefs: Seq[GraphEltDef[D]] = vertexDefs ++ edgeDefs




  def layout(acset: ACSet[D],state:EditorState): ACSet[D] = {
    val es = for 
      edef <- edgeDefs
      ob <- acset.schema.obSeq
      if edef.edgeData.isDefinedAt(ob)
      (d,s,t) = edef.edgeData(ob)
    yield ob -> (s,t)

    _layout(state,assignBends(es.toMap,0.5)(acset))

  }

    
    val spriteSources = eltDefs.map(_.spriteSource)

 
    






// type GraphDefSeq[D] = GraphEltDef[D] | Seq[GraphEltDef[D]]
// // object GraphDisplay:
// //   def apply[D:PartData](defs:GraphDefSeq[D]*) =
// //     val clean = defs.flatMap(ds => ds match
// //         case defs:Seq[GraphEltDef[D]] => defs
// //         case defn:GraphEltDef[D] => Seq(defn)
// //       )
// //     new GraphDisplay(
// //       clean.collect{ case vdef:VertexDef[D] => vdef},
// //       clean.collect{ case edef:EdgeDef[D] => edef}
// //     )


//   // type Data = D
//   // implicit val dataIsPartData: PartData[D] = 
//   //   summon[PartData[D]]
//   // def layout(m: GraphDisplay.this.Model): GraphDisplay.this.Model = ???
//   // implicit val dataIsPartData = dIsData



// // // object GraphDisplay:
// // //   import GraphEltDef._

// //   // def apply[A<:AbstractACSet[A]{ type D = PropMap}](schema:AbstractSchema,specs:GraphEltSpec[PropMap]*): GraphDisplay[A] =
// //   //   val defs = specs.map(GraphEltDef.apply)
// //   //   new GraphDisplay[A](
// //   //     schema,
// //   //     defs.collect{ case v:VertexDef[PropMap] => v},
// //   //     defs.collect{ case e:EdgeDef[PropMap] => e},
// //   //   )

// //     // def apply[A<:AbstractACSet[_A]{ type D = PropMap }]
// //     //   (eltDefs:Seq[GraphEltDef]): GraphDisplay[A] = new GraphDisplay(
// //     //   eltDefs.collect{ case v:VertexDef => v},
// //     //   eltDefs.collect{ case e:EdgeDef => e}
// //     // )
// //     // import SchGraph._

// //     // def apply[
// //     //   D : PartData,
// //     //   A<:AbstractACSet[A]{ type D = D }
// //     // ](
// //     //   schema: AbstractSchema,
// //     //   eltDefs:Seq[
// //     //       /* Vertex defs */
// //     //       schema.Ob | 
// //     //       (schema.Ob,Sprite[PropMap]) | 
// //     //       (schema.Ob,PropMap) | (schema.Ob,PropMap) |
// //     //       (schema.Ob,(Sprite[PropMap],PropMap)) |
// //     //       (schema.Ob,(Sprite[PropMap],PropMap)) |
// //     //       /* Edge defs */
// //     //       (schema.Ob,(schema.Hom,schema.Hom)) | 
// //     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap])) |
// //     //       (schema.Ob,(schema.Hom,schema.Hom,PropMap)) |
// //     //       (schema.Ob,(schema.Hom,schema.Hom,PropMap)) | 
// //     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap], PropMap)) |
// //     //       (schema.Ob,(schema.Hom,schema.Hom,Sprite[PropMap], PropMap))
// //     //     ]
// //     //   ): GraphDisplay[A] =

// //     //   // def mkData(): 
// //     //   implicit val pd: PartData[D] = summon[PartData[D]]
      
// //     //   val defs = eltDefs.map{
// //     //     /* Vertex cases */
// //     //     case ob:schema.Ob => 
// //     //       VertexDef(ob,ShapeNode(),PropMap())
// //     //     case (ob:schema.Ob,sprite:Sprite[D]) => 
// //     //       VertexDef(ob,sprite,PropMap())(pd)
// //     //     case (ob:schema.Ob,init:PropMap) => 
// //     //       VertexDef(ob,ShapeNode(), init)
// //     //     case (ob:schema.Ob,(sprite:Sprite[_],init:PropMap)) => 
// //     //       VertexDef(ob,sprite, init)
        
// //     //     /* Edge defs */
// //     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom)) =>
// //     //       EdgeDef(ob,src,tgt,Arrow(),PropMap())
// //     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,sprite:Sprite[PropMap])) =>
// //     //       EdgeDef(ob,src,tgt,sprite,PropMap())
// //     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,init:PropMap)) =>
// //     //       EdgeDef(ob,src,tgt,Arrow(),init)
// //     //     case (ob:schema.Ob,(src:schema.Hom,tgt:schema.Hom,sprite:Sprite[PropMap],init:PropMap)) =>
// //     //       EdgeDef(ob,src,tgt,sprite,mkData(d))
// //     //   }







// // object GraphEltDef:

// //   // type GraphEltSpec[D] = Ob | //(AOb,Sprite[D]) |
// //   //   (AOb,D) | (AOb,PropMap) | 
// //   //   (AOb,(Sprite[D],D)) | (AOb,(Sprite[D],PropMap)) |
// //   //   (AOb,(AHom,AHom)) | (AOb,(AHom,AHom,Sprite[D])) |
// //   //   (AOb,(AHom,AHom,D)) | (AOb,(AHom,AHom,PropMap)) | 
// //   //   (AOb,(AHom,AHom,Sprite[D],D)) | (AOb,(AHom,AHom,Sprite[D],PropMap))

// //   // def data[D:PartData](p:PropMap) = implicitly[PartData[D]].toData(p)
// //   // def data[D:PartData](): D = data[D](PropMap())

// //   def apply[X<:Ob,D:PartData](ob:X): GraphEltDef[X,D] = 
// //     VertexDef(ob,ShapeNode[D](),PartData[D]())
  
// //   def apply[X<:Ob,D:PartData](ob:X,sprite:Sprite[D]): GraphEltDef[X,D] = 
// //     val init: PropMap = PropMap() + (Bend,2.0)
// //     VertexDef(ob,sprite,PartData[D](init))
  
// //   // def apply[D:PartData](ob:Ob,init:PropMap): GraphEltDef[D] = 
// //   //   val d = PartData[D](init)
// //   //   VertextDef(ob,ShapeNode[D](),d)




// //     // VertexDef(ob,ShapeNode[D](),PartData[D](init))
// //     // VertexDef(ob,ShapeNode(),PartData[D]())
// //     // eltSpec match
// //   //   /* Vertex defs */
// //   //   case ob:AOb => 
// //   //     VertexDef(ob,ShapeNode(),data())
// //   //   case (ob:AOb,spec) => spec match
// //   //     case spr:Sprite[D] => 
// //   //       VertexDef(ob,spr,data())
// //   //     case init:D => 
// //   //       VertexDef(ob,ShapeNode(),init)
// //   //     case init:PropMap => 
// //   //       VertexDef(ob,ShapeNode(),data(init))
// //   //     case (spr:Sprite[D],init:D) => 
// //   //       VertexDef(ob,spr,init)
// //   //     case (spr:Sprite[D],init:PropMap) => 
// //   //       VertexDef(ob,spr,data(init))
// //   //   /* Edge defs */
// //   //     case (src:AHom,tgt:AHom) =>
// //   //       EdgeDef(ob,src,tgt,Arrow(),data())
// //   //     case (src:AHom,tgt:AHom,spr:Sprite[D]) =>
// //   //       EdgeDef(ob,src,tgt,spr,data())        
// //   //     case (src:AHom,tgt:AHom,init:D) =>
// //   //       EdgeDef(ob,src,tgt,Arrow(),init)
// //   //     case (src:AHom,tgt:AHom,init:PropMap) =>
// //   //       EdgeDef(ob,src,tgt,Arrow(),data(init))
// //   //     case (src:AHom,tgt:AHom,spr:Sprite[D],init:D) =>
// //   //       EdgeDef(ob,src,tgt,spr,init)
// //   //     case (src:AHom,tgt:AHom,spr:Sprite[D],init:PropMap) =>
// //   //       EdgeDef(ob,src,tgt,spr,data(init))




