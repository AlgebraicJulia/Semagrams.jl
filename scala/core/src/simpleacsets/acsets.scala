package semagrams.simpleacsets

import semagrams._
import semagrams.acsets.Id
import cats.data.State
// import scala.collection.mutable
import upickle.default._
import semagrams.util.Complex
import monocle.Lens
import semagrams.acsets.Petris.T

import scala.util.Random
// import semagrams.util.msgError
// import semagrams.util.bijectiveRW
// import semagrams.util.Complex
// import semagrams.acsets.catlab.Ob


// trait Doctrine:
//   def generators: Seq[Simple[CatType]]
//   def constructors: Seq[CatType]


// case object CatDoctrine extends Doctrine:
//   val 
// case object SchemaDoctrine extends Doctrine




/** Abstract Categorical structure **/

/* General categorical entities (e.g., `Ob`, `Arrow`) */
sealed trait CatElt:
  def generators: Seq[SimpleElt]
  def label: String
  override def toString = label

/* Atomic entities */
trait SimpleElt extends CatElt:
  def name: String
  def label = name

/* Trait for `CatElt` companions */
// sealed trait CatType:
//   type ScalaType
//   def contains(elt:CatElt): Boolean = elt.isInstanceOf[ScalaType]
  

sealed trait Ob extends acsets.Ob with CatElt:
  def id = IdArrow(this)

sealed trait Arrow extends CatElt:
  type DomType <: Ob
  type CodomType <: Ob

  val dom: DomType
  val codom: CodomType

  def path: Seq[Arrow]
  

/** Abstract Schemas **/


sealed trait SchemaElt extends CatElt:
  override def generators: Seq[SchemaElt & SimpleElt]

/** Categorical typing **/
/* Schema objects */
sealed trait SchemaOb extends Ob with SchemaElt
sealed trait TableOb extends SchemaOb derives ReadWriter
sealed trait TypeOb[T:ReadWriter] extends SchemaOb


/* Schema arrows */

sealed trait SchemaArrow extends Arrow with SchemaElt with Property

sealed trait Hom(dom:TableOb,codom:TableOb) extends SchemaArrow:
  type DomType = TableOb
  type CodomType = TableOb

sealed trait Observable[T:ReadWriter](dom:TableOb,codom:TypeOb[T]) extends SchemaArrow:
  type DomType = TableOb
  type CodomType = TypeOb[T]

sealed trait Comp[S:ReadWriter,T:ReadWriter](dom:TypeOb[S],codom:TypeOb[T]) extends SchemaArrow:
  type DomType = TypeOb[S]
  type CodomType = TypeOb[T]



/** Generators **/


// sealed trait Table extends Simple[Ob]
//   derives ReadWriter:
//   def generators: Seq[Table] = Seq(this)

// sealed trait SimpleArrow extends SimpleElt with Arrow
//   with Property:
//   def generators = dom.generators ++ codom.generators :+ this
  
/* Object generators */
case class Table(name:String) extends TableOb with SimpleElt:

  def generators: Seq[Table] = Seq(this)
  

object Table:
  def apply(ob:acsets.Ob) = new Table(ob.toString())
  def apply() = new Table(makeId("Table"))
  def apply(ob:Option[acsets.Ob| String]): Table = ob match
    case Some(s:String) => new Table(s)
    case Some(ob:acsets.Ob) => Table(ob)
    case None => Table()

  

def makeId(prefix:String) = prefix + Random.nextInt(10000).toString

case class ValType[T:ReadWriter](name:String) extends TypeOb[T] with SimpleElt:
  def generators: Seq[ValType[_]] = Seq(this)

/* Arrow generators */
case class FKey(name:String,dom:TableOb,codom:TableOb) 
  extends Hom(dom,codom) with SimpleElt with acsets.Hom:
    val doms = dom.asDom()
    val codoms = codom.asDom()
    val path: Seq[FKey] = Seq(this)
    val generators: Seq[FKey] = Seq(this)

case class Attr[T:ReadWriter](name:String,dom:TableOb,codom:TypeOb[T]) 
  extends Observable(dom,codom) with SimpleElt with acsets.Attr:
    type Value = T
    val doms = dom.asDom()
    val path: Seq[Attr[_]] = Seq(this)
    val generators: Seq[Attr[_]] = Seq(this)

    val rw: ReadWriter[T] = summon[ReadWriter[T]]



/* Object Constructors */

case object UnitType extends Ob:
  def generators = Seq()
  def label = "I"

/* Arrow constructors */

/* Identities */
case class IdArrow[X<:Ob](ob:X) extends Arrow:
  type DomType = X
  type CodomType = X
  val dom = ob
  val codom = ob 
  def generators = ob.generators
  def label = s"id(${ob.label})"

  override def path = Seq() 

/* Composition */
case class SeqArrow[X<:Ob,Y<:Ob](dom:X,codom:Y,override val path:Seq[Arrow]) 
  extends Arrow:
    type DomType = X
    type CodomType = Y
    assert(path.length > 1)
    assert(dom +: path.map(_.codom) == path.map(_.dom) :+ codom)
    def generators = path.flatMap(_.generators).distinct
    def label = path match
      case Seq() => "I"
      case _ => path.map(_.label).mkString("⋅")



// /** A part is a path through a nested acset. If you visualize a nested acset as
//   * a tree, where each node is an acset and its children are all of its
//   * subacsets, then a part tells at each level which subacset to choose.
//   *
//   * The empty path refers to the root acset.
//   *
//   * @todo
//   *   We should have "relative" partProps and "absolute" partProps, just like in a
//   *   filesystem there are relative and absolute paths. I think that currently
//   *   `acsets.Part` is used for both notions, which is very confusing.
//   */
case class Part(ob:Table,idTag:Id) 
  extends Entity derives ReadWriter {

  def asPart = acsets.Part(Seq((ob,idTag)))
  def id = idTag.id

  /** All of the objects of the path */
  override val ty: acsets.PartType = asPart.ty


  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.toString() + id.toString()
}

object Part:
  def apply(ob:Table,i:Int) = new Part(ob,Id(i))
  def apply(part:acsets.Part) = new Part(
    Table(part.lastOb),
    part.lastId
  )



case class SimpleSchema(
  obs: Seq[Table],
  homs: Seq[FKey],
  attrs: Seq[Attr[_]],
  override val props: Seq[Property]
) extends acsets.Schema:
  
  def generators: Seq[SimpleElt & SchemaElt] = obs ++ homs// ++ attrs

  def ++(elts:Seq[SchemaElt]) =
    val simpleElts = elts.flatMap(_.generators).distinct.filterNot(generators.contains)
    this.copy(
      obs = obs ++ simpleElts.collect{case x:Table => x},
      homs = homs ++ simpleElts.collect{case f:FKey => f},
      attrs = attrs ++ simpleElts.collect{case a:Attr[_] => a}
    )
  
  def +(elt:SchemaElt) = ++(elt.generators)

  def ++(S:SimpleSchema): SimpleSchema = ++(S.generators).copy(
    props = (props ++ S.props).distinct 
  )

  override def toString = "SimpleSchema(" + 
    (if obs.isEmpty then "" else 
      "\n  Ob:   " + obs.mkString(", ")
    ) + (if homs.isEmpty then "" else 
      "\n  Hom:  " + homs.mkString(", ")
    ) + (if attrs.isEmpty then "" else 
      "\n  Observable: " + attrs.mkString(", ")
    ) + (if props.isEmpty then "" else 
      "\n  Prop: " + props.mkString(", ")
    ) + (if (generators ++ props).isEmpty then ")\n" else "\n)\n")


object SimpleSchema:
  def apply(elts:CatElt*): SimpleSchema =
    SimpleSchema(Seq(),elts:_*)

  def apply(globalProps:Seq[Property],elts:CatElt*) =
    val gens = elts.flatMap(_.generators)
    new SimpleSchema(
      gens.collect{ case x:Table => x},
      gens.collect{ case f:FKey => f},
      gens.collect{ case a:Attr[_] => a},
      globalProps
    )






type PropStore = Map[Id,PropMap]

// extension (pstore:PropStore)
//   def remProp(f:Property,id:Id): PropStore =
//     pstore + (id -> (pstore(id) - f))
//   def remProp(f:Property,p:Part):PropStore = remProp(f,p.idTag)



// /** Storage class for the partProps corresponding to an `Ob` in a schema.
//   *
//   * This is immutable; methods that logically mutate simply return new objects.
//   *
//   * @param nextId
//   *   The id to assign to the next part added
//   *
//   * @param ids
//   *   The ids of all the partProps added so far. This is a `Seq` because we care
//   *   about the ordering; when the ACSet is displayed this ordering is used when
//   *   two sprites overlap.
//   *
//   * @param acsets
//   *   The subacset corresponding to each id
//   */
case class PartSet(
  nextId: Int,
  ids: Seq[Id],
  propStore: PropStore
) {

  /** Add multiple new partProps, with subacsets given in `partacsets`.
    *
    * Returns a sequence of the ids of the added partProps.
    */
  def addParts(partProps: Seq[PropMap]): (PartSet, Seq[Id]) = {
    val newIds = nextId.to(nextId + partProps.length - 1).map(Id.apply)
    val newParts = PartSet(
      nextId + partProps.length,
      ids ++ newIds,
      propStore ++ newIds.zip(partProps).toMap
    )
    (newParts, newIds)
  }

  /** Adds a single part with subacset `acs`, returns its id. */
  def addPart(props: PropMap): (PartSet, Id) = {
    val (p, newIds) = addParts(Seq(props))
    (p, newIds(0))
  }

  /** Set the subacset corresponding to `i` to `acs` */
  def setProps(i: Id, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> props)
    )
  }

  def softSetProps(i: Id, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> (props ++ propStore(i)))
    )
  }

  /** Remove the part with id `i` */
  def remPart(i: Id) = {
    this.copy(
      ids = ids.filterNot(_ == i),
      propStore = propStore.filterNot(_._1 == i)
    )
  }

  def remProp(f:Property,i: Id) = {
    this.copy(
      propStore = propStore + (i -> (propStore(i) - f))
    )
  }

  /** Move the id `i` to the front of the list of ids.
    *
    * This is used, for instance, when dragging a sprite so that the sprite goes
    * over the other partProps.
    */
  def moveFront(i: Id) = {
    this.copy(
      ids = ids.filterNot(_ == i) :+ i
    )
  }

  /** Move the id `i` to the index `j` in the list of ids.
    *
    * This is used, for instance, when setting the position of a port.
    */
  def moveToIndex(i: Id, j: Int) = {
    val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
    this.copy(
      ids = (seg1 :+ i) ++ seg2
    )
  }

  def contains(id:Id) = ids.contains(id)
  def contains(i:Int) = ids.contains(Id(i))


}
object PartSet:
  /* Validataion */
  def apply(n:Int,ids:Seq[Id],propStore:PropStore): PartSet =
    assert(n >= ids.map(_.id).max)
    assert(ids.toSet == propStore.keySet)
    new PartSet(n,ids,propStore.withDefaultValue(PropMap()))
  
  /* Convenience methods */
  def apply(ids:Seq[Id],propStore:PropStore): PartSet = 
    PartSet(ids.length,ids,propStore)
  def apply(propStore:PropStore): PartSet =
    PartSet(propStore.keys.toSeq,propStore)
  def apply(ids:Seq[Id]): PartSet =
    PartSet(ids.length,ids,ids.map((_,PropMap())).toMap)
  def apply(n:Int): PartSet =
    PartSet((0 until n).map(Id(_)))
  def apply(): PartSet = PartSet(0)





type PartStore = Map[Ob,PartSet]
extension (pstore:PartStore)
  def contains(p:Part) = pstore(p.ob).contains(p.id)
  def remProp(f:Property,p:Part) = 
    pstore + (p.ob -> pstore(p.ob).remProp(f,p.idTag)
  )

object PartStore:
  def apply() = Map[Ob,PartSet]().withDefaultValue(PartSet())
  

// /** The part corresponding to the top-level acset itself. */

// /** The empty schema. */
val SchEmpty = SimpleSchema(Seq(),Seq(),Seq(),Seq())

// /** A nested acset.
//   *
//   * @param schema
//   *   the schema that the acset conforms to
//   *
//   * @param props
//   *   the top-level properties. The values of morphisms, attributes, and generic
//   *   [[Property]]s are stored here. We don't need what in Catlab we call
//   *   "subparts"; it's folded into this. For instance, if this is the subacset
//   *   for an edge, then the source and target are stored here.
//   *
//   * @param partsMap
//   *   the `PartSet` object for each `Ob` in the schema. This is where the
//   *   subacsets are stored.
//   */
case class ACSet(
  schema: SimpleSchema,
  globalProps: PropMap,
  partStore: PartStore
) {

  /* Parts */

  /** Check if a part exists in the ACSet */
  def hasPart(p: Part): Boolean = partStore.contains(p)
  def hasPart(ob:Ob,id:Id): Boolean = ob match
    case ob:Table => hasPart(Part(ob,id))
    case _ => false
  def hasPart(ob:Ob,i:Int): Boolean = hasPart(ob,Id(i))

  def partProps(ob:Ob) = parts(ob).map(part => 
    part -> getProps(part)
  )
  def allPartProps = schema.obs.flatMap(partProps)

  def parts(ob:Ob) = ob match
    case ob:Table => partStore(ob).ids.map(Part(ob,_))
    case _ => Seq()
  def allParts = schema.obs.flatMap(parts)
  def apply(ob:Ob) = parts(ob)

  def hasProp(f:Property,p:Part) = 
    hasPart(p) & getProps(p).contains(f)
  def hasProps(fs:Iterator[Property],p:Part) = 
    fs.forall(hasProp(_,p))
  
  
  def getProps(p:Part): PropMap = 
    partStore(p.ob).propStore(p.idTag)
  def getProps(fs:Seq[Property],p:Part): PropMap = 
    PropMap(fs.map(f => f -> getProp(f,p)).toMap)
  
  
  def getProp(f:Property,p:Part): f.Value = tryProp(f,p).get
  def getProp(f:Property,ob:Ob): Seq[f.Value] = 
    parts(ob).map(part => getProp(f,part))
  def getProp(f:SchemaArrow): Seq[f.Value] = getProp(f,f.dom)

  def tryProp(f:Property,p:Part): Option[f.Value] = 
    getProps(p).get(f)
  def tryProp(f:Property,ob:Ob): Seq[Option[f.Value]] = 
    parts(ob).map(tryProp(f,_))
  def tryProp(f:SchemaArrow): Seq[Option[f.Value]] = tryProp(f,f.dom)

  def collectProp(f:Property,ps:Seq[Part]): Seq[(Part,f.Value)] =
    ps.filter(hasProp(f,_))
    .map(part => part -> getProp(f,part))
  def collectProp(f:Property,ob:Ob): Seq[(Part,f.Value)] =
    collectProp(f,parts(ob))
  def collectProp(f:SchemaArrow): Seq[(Part,f.Value)] =
    collectProp(f,f.dom)


  def collectProps(fs:Seq[Property],ps:Seq[Part]): Seq[(Part,PropMap)] =
    ps.filter(getProps(_).contains(fs))  
    .map(part => (part,getProps(fs,part)))
  def collectProps(fs:Seq[Property],ob:Ob): Seq[(Part,PropMap)] =
    collectProps(fs,parts(ob))

  /** Set the subacset for a nested part */
  def setProps(p: Part, props: PropMap): ACSet = this.copy(
    partStore = partStore + (p.ob -> 
      partStore(p.ob).copy(
        propStore = partStore(p.ob).propStore + (p.idTag -> props)
      )
    )
  )
  def setProps(kvs:Seq[(Part,PropMap)]): ACSet = kvs match
    case Seq() => this
    case (part,props) +: tail => 
      setProps(part,props).setProps(tail)
  
  def setProps(ps:Seq[Part],props:PropMap): ACSet = 
    setProps(ps.map((_,props)))
  

  def setProps(ob:Ob,props:PropMap): ACSet = 
    setProps(parts(ob),props)

  def setProp(f:Property,p:Part,v:f.Value): ACSet = 
    setProps(p,getProps(p).set(f,v))

  def setProp(f:Property,kvs:Seq[(Part,f.Value)]): ACSet = kvs match
    case Seq() => this
    case (part,v) +: tail => setProp(f,part,v).setProp(f,tail)

  def setProp(f:Property,ps:Seq[Part],v:f.Value): ACSet =
    setProp(f,ps.map((_,v)))


  def softSetProps(p:Part,props:PropMap): ACSet =
    setProps(p,props ++ getProps(p))

  def softSetProps(kvs:Seq[(Part,PropMap)]): ACSet = kvs match
    case Seq() => this
    case (part,props) +: tail =>
      softSetProps(part,props).softSetProps(tail)

  def softSetProps(ps:Seq[Part],props:PropMap): ACSet =
    softSetProps(ps.map((_,props)))

  def softSetProps(ob:Ob,props:PropMap): ACSet = 
    softSetProps(parts(ob),props)
  
    
  def softSetProp(f:Property,p:Part,v:f.Value): ACSet = 
    if hasProp(f,p)
    then this
    else setProp(f,p,v)

  def softSetProp(f:Property,kvs:Seq[(Part,f.Value)]): ACSet = kvs match
    case Seq() => this
    case (part,v) +: tail => softSetProp(f,part,v).softSetProp(f,tail)

  def softSetProp(f:Property,ps:Seq[Part],v:f.Value): ACSet =
    softSetProp(f,ps.map((_,v)))


  /** Adds a part of type `x` to the subacset at `p` with initial subacset
    * `init`
    */
  def addPart(ob: Table, props: PropMap = PropMap()): (ACSet, Part) = 
    val (partSet,newId) = partStore(ob).addPart(props)
    val acset = this.copy(
      partStore = partStore + (ob -> partSet)
    )
    (acset,Part(ob,newId))

  /** Add several partProps of type `x` to the subacset at `p` with initial
    * subacsets given by inits.
    */
  def addParts(ob:Table,propSeq:Seq[PropMap]): (ACSet,Seq[Part]) = 
    addParts(ob,propSeq,Seq())
  def addParts(ob: Table, propSeq: Seq[PropMap], partSeq:Seq[Part]): (ACSet, Seq[Part]) = 
    propSeq match
      case Seq() => (this,partSeq)
      case head +: tail =>
        val (next,part) = addPart(ob,head)
        next.addParts(ob,tail,partSeq :+ part)
  
  def addParts(x:Table,n:Int,props:PropMap = PropMap()): (ACSet,Seq[Part]) =
    addParts(x,Seq.fill(n)(props)) 
  



  // /** Move the part `p` to the front of its parent `PartSet`. See
  //   * [[PartSet.moveFront]].
  //   */
  def moveFront(p: Part): ACSet = {
    this.copy(
      partStore = partStore + (p.ob -> partStore(p.ob).moveFront(p.idTag))
    )
  }

  // /** Move the part `p` to the front of its parent `PartSet`. See
  //   * [[PartSet.moveFront]].
  //   */
  def moveToIndex(p: Part, idx: Int): ACSet = {
    this.copy(
      partStore = partStore + (p.ob -> 
        partStore(p.ob).moveToIndex(p.idTag,idx)
      )
    )
  }


  // /** Set the properties `pm` for all partProps of type `ob` **/
  def setGlobalProps(props:PropMap): ACSet = this.copy(
    globalProps = props
  )

  def softSetGlobalProps(props:PropMap): ACSet = this.copy(
    globalProps = props ++ globalProps
  )

  def setGlobalProp(f:Property,v:f.Value) = setGlobalProps(
    globalProps.set(f,v)
  )
  def softSetGlobalProp(f:Property,v:f.Value): ACSet = 
    softSetGlobalProps(PropMap(f -> v))
  
  def getGlobalProps(fs:Seq[Property]) = 
    PropMap(fs.map(f => (f,globalProps(f))).toMap)

  def getGlobalProp(f:Property) = globalProps(f)

  def tryGlobalProps(fs:Seq[Property]) =
    fs.map(f => (f,globalProps.get(f)))

  def collectGlobalProps(fs:Seq[Property]) = PropMap(
    (for f <- fs; v <- globalProps.get(f) yield (f,v))
      .toMap
  )


  // /** Return sequence of the partProps that have property `f` set to `p` */
  def fiber(f:SchemaArrow,v:f.Value): Seq[Part] =
    collectProp(f).filter(_._2 == v).map(_._1)
  def fiber(ob:Ob,f:Property,v:f.Value) =
    collectProp(f,ob).filter(_._2 == v).map(_._1)

  def fibers(ps:Seq[Part],f:Property): Map[f.Value,Seq[Part]] =
    ps.foldLeft(
      Map[f.Value,Seq[Part]]().withDefaultValue(Seq())
    )( (fibs,part) => tryProp(f,part) match
      case Some(v) => fibs + (v -> (fibs(v) :+ part))
      case None => fibs
    )
  def fibers(f:SchemaArrow):Map[f.Value,Seq[Part]] = fibers(parts(f.dom),f)
  
  def globalFiber(v0:Any): Seq[(Part,Property)] = for 
    (pt,props) <- allPartProps
    (f,v) <- props.pmap
    if v == v0
  yield (pt,f)


  def remGlobalProp(f:Property): ACSet = {
    this.copy(
      globalProps = globalProps - f
    )
  }
  
  def remProp(f:Property,p:Part) = this.copy(
    partStore = partStore.remProp(f,p)    
  )

  def remProp(f:Property,ps:Seq[Part]): ACSet = ps match
    case Seq() => this
    case head +: tail => remProp(f,head).remProp(f,tail)
  
  def remProp(f:Property,ob:Ob): ACSet =
    remProp(f,parts(ob))

  def remProp(f:SchemaArrow): ACSet =
    remProp(f,f.dom)

  def remProps(kvs:Seq[(Part,Property)]): ACSet = kvs match
    case Seq() => this
    case (part,f) +: tail => remProp(f,part).remProps(tail)
  

  /** Remove a part, but not any of the other partProps that might refer to it. */
  def remPart(p: Part): ACSet = 
    val refs = globalFiber(p)
    remProps(refs).copy(
      partStore = partStore + (p.ob -> 
        partStore(p.ob).remPart(p.idTag)
      )
    )

  def remParts(ps:Seq[Part]): ACSet = ps match
    case Seq() => this
    case head +: tail => remPart(head).remParts(tail)
  

  // /** Remove a part and all of the other partProps that refer to it. */
  def remPartsCascade(ps: Seq[Part]): ACSet = ps match
    case Seq() => this
    case ps => 
      val fibs = ps.flatMap(globalFiber(_)) 
      remParts(ps).remPartsCascade(fibs.map(_._1))

  def remPartCascade(p:Part): ACSet = remPartsCascade(Seq(p))
  


  def scale(
      from: Complex,
      to: Complex,
      scaleProps: Seq[Property { type Value = Complex }] = Seq(Center),
      scaleGlobal: Seq[Property{ type Value = Complex }] = Seq()
  ): ACSet =

    val newGlobals = globalProps.scale(scaleGlobal,from,to)

    val newParts = partStore.map((ob,partSet) => (ob ->
      partSet.copy(
        propStore = partSet.propStore.map((id,props) => (id ->
          props.scale(scaleProps,from,to)
        ))
      )
    ))
    
    this.copy(
      globalProps = newGlobals,
      partStore = newParts
    )

}

/** This object contains the constructor method for ACSets and also a collection
  * of wrappers around ACSet methods in the `State` monad that allow for a
  * quasi-imperative API for modifying ACSets purely.
  */
object ACSet {

  /** Construct a new ACSet with schema `s` */
  def apply(s: SimpleSchema): ACSet = ACSet(s, PropMap())

  /** Construct a new ACSet with schema `s` and top-level partProps `props` */
  def apply(s: SimpleSchema, props: PropMap): ACSet =
    val pm = s.obs.map(ob => ob -> PartSet(0, Seq(), Map())).toMap
    new ACSet(s, props, PartStore())

  /** `State` wrapper around ACSet.addParts */
  def addParts(x: Table, props: Seq[PropMap]): State[ACSet, Seq[Part]] =
    State(_.addParts(x, props))

  /** `State` wrapper around ACSet.addPart */
  def addPart(x: Table, props: PropMap = PropMap()): State[ACSet, Part] =
    State(_.addPart(x, props))

  /** `State` wrapper around ACSet.setSubpart */
  def setProp(f: Property, p: Part, v: f.Value): State[ACSet, Unit] =
    State.modify(_.setProp(f, p, v))

  /** `State` wrapper around ACSet.remSubpart */
  def remProp(f: Property, p: Part): State[ACSet, Unit] =
    State.modify(_.remProp(f,p))

  /** `State` wrapper around ACSet.remPart */
  def remPart(p: Part): State[ACSet, Unit] = State.modify(_.remPart(p))

  /** `State` wrapper around ACSet.remParts */
  def remParts(ps: Seq[Part]): State[ACSet, Unit] = State.modify(_.remParts(ps))

  /** `State` wrapper around ACSet.moveFront */
  def moveFront(p: Part): State[ACSet, Unit] = State.modify(_.moveFront(p))

  /** Returns a lens into the value of the property `f` for part `x` */
  def subpartLens(f: Property, x: Part) =
    Lens[ACSet, f.Value](_.getProp(f, x))(y => s => s.setProp(f, x, y))
}




// case class AddPartMsg(ob:Ob,props:PropMap = PropMap()) extends Message[ACSet] {
//   def execute(a:ACSet) = a.addPart(ob,props)._1
// }

// case class RemovePartMsg(part:Part) extends Message[ACSet] {
//   def execute(a:ACSet) = a.remPart(part)
// }

// case class SetSubpartMsg(part:Part,prop:Property)(v:prop.Value) extends Message[ACSet] {
//   def execute(a:ACSet) = a.setSubpart(part,prop,v)
// }

// case class RemoveSubpartMsg(part:Part,prop:Property) extends Message[ACSet] {
//   def execute(a:ACSet) = a.remSubpart(part,prop)
// }

