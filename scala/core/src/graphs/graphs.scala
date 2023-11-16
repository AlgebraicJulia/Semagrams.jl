package semagrams.graphs

import upickle.default._
import com.raquo.laminar.api.L._


import semagrams._
import semagrams.rendering._
import semagrams.util._
// import semagrams.{Generator, given, Ob, PartHom, Hom, PValue, PropMap, PartProp}





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

enum GraphProp[T:ReadWriter] extends PValue[T]:
  case SrcName extends GraphProp[String]
  case TgtName extends GraphProp[String]
export GraphProp._

case object SchGraph extends Schema:

  val id = UUID("SchGraph")

  var name = "SchGraph"

  def elts = GraphOb.values.eltMap ++ GraphHom.values.eltMap
  def globalProps = Seq()





object Graph {
  def apply() = ACSet[PropMap](SchGraph)
}




sealed trait GraphEltDef[D:PartData]:
  
  def sprite: PartialFunction[Ob,Sprite[D]]
  
  def init: PartialFunction[Ob,D]
  def test = init.isDefinedAt

  def entitySource: EntitySource[ACSet[D],D]


object GraphEltDef:
  def makeSpriteSource[D:PartData](defn:GraphEltDef[D]): EntitySource[ACSet[D],D] =
    EntitySource[ACSet[D],D]( (acset,emap) => 
      val triples = acset.schema.obSeq
        .filter(defn.test)
        .flatMap(ob => 
          acset.getDataSeq(ob).map((part,data) =>
            part -> (defn.sprite(ob),defn.init(ob).merge(data))  
          )        
        )
      triples.reverse
    )

    



/* Definition data for a vertex type */
type VDef[D] = (Sprite[D],D)


case class VertexDef[D:PartData](
  defn: PartialFunction[Ob,VDef[D]]
) extends GraphEltDef[D]:

  def init = defn.andThen(_._2)
  def sprite = defn.andThen(_._1)
  def entitySource: EntitySource[ACSet[D],D] = 
    GraphEltDef.makeSpriteSource(this)

  

/* Possible input data for a vertex type */
type VData[D] = VDef[D] | Sprite[D]

/* Convert partial input data into a full definition */
def vdef[D:PartData](kv:(Ob,VData[D])): (Ob,VDef[D]) = kv match
  case (ob,vdef:VDef[D]) => ob -> vdef
  case (ob,sprite:Sprite[D]) => ob -> (sprite,PartData())



object VertexDef:
  def apply[D:PartData](ob:Ob,sprite:Sprite[D]): VertexDef[D] =
    new VertexDef(Map(ob -> (sprite,PartData())))
  def apply[D:PartData](ob:Ob,sprite:Sprite[D],init:D): VertexDef[D] =
    new VertexDef(Map(ob -> (sprite,init)))
  def apply[D:PartData,VD<:VData[D]](vdata:(Ob,VD)*): VertexDef[D] =
    new VertexDef(vdata.map(vdef).toMap)


/* Definition data for an edge type */
type EDef[D] = (PartHom,PartHom,Sprite[D],D)


case class EdgeDef[D:PartData](
  defn: PartialFunction[Ob,EDef[D]]
) extends GraphEltDef[D]:
  
  val sprite = defn.andThen(_._3)
  val init = defn.andThen(_._4)

  def entitySource: EntitySource[ACSet[D],D] = 
    GraphEltDef.makeSpriteSource(this)
      .addPropsBy( (part,entitymap,data) =>
        val (src,tgt,_,_) = defn(part.ob) 
        edgeProps[D](src,tgt)(part,data,entitymap)
      )



/* Possible input data for an edge type */
type EData[D] = EDef[D] | (PartHom,PartHom,Sprite[D])

/* Convert partial input data into a full definition */
def edef[D:PartData](kv:(Ob,EData[D])): (Ob,EDef[D]) =
  val thedef = kv match
    case (ob,edef:EDef[D]) => ob -> edef
    case (ob,(src:PartHom,tgt:PartHom,sprite:Sprite[D])) => ob -> (src,tgt,sprite,PartData())

  val (ob,(src,tgt,_,_)) = thedef
  assert(ob == src.dom & ob == tgt.dom)
  thedef 
  

object EdgeDef:
  def apply[D:PartData](ob:Ob,src:PartHom,tgt:PartHom,sprite:Sprite[D]): EdgeDef[D] =
    EdgeDef(ob -> (src,tgt,sprite))
  def apply[D:PartData](ob:Ob,src:PartHom,tgt:PartHom,sprite:Sprite[D],init:D): EdgeDef[D] =
    EdgeDef[D,EData[D]](ob -> (src,tgt,sprite,init))
  def apply[D:PartData,ED<:EData[D]](kvs:(Ob,ED)*): EdgeDef[D] =
    new EdgeDef(kvs.map(edef).toMap)


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


  



case class GraphDisplay[D:PartData](
  override val modelVar: UndoableVar[ACSet[D]],
  override val bindings: Seq[Binding[ACSet[D]]],
  vertexDef: VertexDef[D],
  edgeDef: EdgeDef[D],
  _layout: (EditorState,ACSet[D]) => ACSet[D]
) extends ACSemagram[D]:




  def layout(acset: ACSet[D],state:EditorState): ACSet[D] = {
    val vs = for 
      ob <- acset.schema.obSeq
      if vertexDef.defn.isDefinedAt(ob)
    yield ob

    val es = for
      ob <- acset.schema.obSeq
      if edgeDef.defn.isDefinedAt(ob)
      (s,t,_,_) = edgeDef.defn(ob)
    yield ob -> (s,t)

    val custom = _layout(state,acset)
    val withBends = assignBends(es.toMap,0.5)(custom)
    val withNames = withBends
      .softSetProp(Content,
        vs.flatMap(ob => acset.mapParts(ob,_.toString)
      ))
      .softSetProp(SrcName,
        es.flatMap{ case (ob,(src,tgt)) =>
          acset.mapProp(src,_.toString)
        }
      )
      .softSetProp(TgtName,
        es.flatMap{ case (ob,(src,tgt)) =>
          acset.mapProp(src,_.toString)
        }
      )


    withNames


  }

    
    val entitySources = Seq(vertexDef.entitySource,edgeDef.entitySource)

 
    


object GraphDisplay:

  def defaultLayout[D:PartData](es:EditorState,acset:ACSet[D]): ACSet[D] =    
    acset.softSetProp(Highlight,es.hoveredPart,())
      .softSetProp(Selected,es.selected,())


  def apply[D:PartData](
    modelVar: UndoableVar[ACSet[D]],
    bindings: Seq[Binding[ACSet[D]]],
    vertexDef: VertexDef[D],
    edgeDef: EdgeDef[D],
  ) = new GraphDisplay(modelVar,bindings,vertexDef,edgeDef,defaultLayout)


