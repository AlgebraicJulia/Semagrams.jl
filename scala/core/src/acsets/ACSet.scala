package semagrams.acsets



import semagrams._
import semagrams.util._

import upickle.default._
import scala.annotation.targetName


case class ACSet[D:PartData](
  name: String,
  schema:Schema,
  globalProps: PropMap,
  partStore: PartStore[D]
):

  /** Getting **/


  /* Getting aggregate data by object */ 

  /* Get an ordered sequence of parts from an `ob` */
  def getParts(ob:Ob): Seq[Part] = partStore(ob.id).ids.map(Part(_,ob))


  /* Get an ordered sequence of parts and `PartData` from `ob` */
  def getDataSeq(ob:Ob): Seq[(Part,D)] = getParts(ob).map(part => part -> partStore(ob.id).getData(part.id))

  /* Get an ordered sequence of parts and `PropMap`s from `ob` */
  def getPropSeq(ob:Ob) = getDataSeq(ob).map((part,data) => part -> data.getProps())  

  /* Get a map of parts to `PartData` from `ob` */
  def getData(ob:Ob) = getDataSeq(ob).toMap

  /* Get a map of parts to `PropMap`s from `ob` */
  def getProps(ob:Ob) = getPropSeq(ob).toMap
  
  /* Getting individual properties by object */


  /* Get `PartData` for a single `part` (or empty) */
  def getData(part:Part): D = partStore(part.ob.id).dataStore(part.id)

  /* Get `PartData` for a collection of `parts` */
  def getData(parts:Iterable[Part]): Map[Part,D] = 
    parts.groupBy(_.ob).foldLeft(Map[Part,D]()){
      case (data,(ob,obparts)) => data ++ parts.map(part =>
        part -> getData(part)
      ).toMap
    }
    

  /* Get `PropMap` for a single `part` */
  def getProps(part:Part): PropMap = getData(part).getProps()

  /* Get `PropMap`s for a collection `parts` */
  def getProps(parts:Iterable[Part]): Iterable[(Part,PropMap)] = 
    getData(parts).map(_ -> _.getProps())

  
  /* Get an optional value for `f` at `part` */
  def tryProp(f:Property,part:Part) = partStore(part.ob.id).dataStore(part.id).tryProp(f)

  /* Get the value for `f` at `part` (unsafe) */
  def getProp(f:Property,part:Part) = tryProp(f,part).get

  /* Check if `f` is defined at `part` */
  def hasProp(f:Property,part:Part) = tryProp(f,part).isDefined
  

  /* Get a map of optional values for `f`*/
  def tryProp(f:Property,parts:Iterable[Part]): Map[Part,Option[f.Value]] = parts.map(part => part -> tryProp(f,part)).toMap
  def tryProp(f:Property,ob:Ob): Map[Part,Option[f.Value]] = tryProp(f,getParts(ob))
  def tryProp(f:Hom[_,_]): Map[Part,Option[f.Value]] = tryProp(f,f.dom)
    
  /* Get a map of available values for `f` */
  def collectProp(f:Property,parts:Iterable[Part]) = tryProp(f,parts).collect{ case part -> Some(v) => part -> v }
  def collectProp(f:Property,ob:Ob): Map[Part,f.Value] = collectProp(f,getParts(ob))
  def collectProp(f:Hom[_,_]): Map[Part,f.Value] = collectProp(f,f.dom)

  /* Map the available values `f` on `parts` using `g` */
  def mapProp[A](f:Property,parts:Iterable[Part],g:f.Value => A): Map[Part,A] =
    collectProp(f,parts).map((part,fval) => part -> g(fval))

  /* Map the optional values `f` on `parts` using `g` */
  @targetName("mapPropOption")
  def mapProp[A](f:Property,parts:Iterable[Part],g:Option[f.Value] => A): Map[Part,A] =
    tryProp(f,parts).map((part,fval) => part -> g(fval))

  def mapProp[A](f:Property,ob:Ob,g:f.Value => A): Map[Part,A] =
    collectProp(f,ob).map((part,fval) => part -> g(fval))

  def mapProp[A](f:Hom[_,_],g:f.Value => A): Map[Part,A] =
    mapProp(f,f.dom,g)  

  /* Filter the available values of `f` on `parts` according to `pred` */
  def filterProp(f:Property,parts:Iterable[Part],pred:f.Value => Boolean): Map[Part,f.Value] =
    collectProp(f,parts).filter((_,fval) => pred(fval))
  
  def filterProp(f:Property,ob:Ob,pred:f.Value => Boolean): Map[Part,f.Value] =
    collectProp(f,ob).filter((_,fval) => pred(fval))
  
  def filterProp(f:Hom[_,_],pred:f.Value => Boolean): Map[Part,f.Value] =
    filterProp(f,f.dom,pred)
    
  /* Map the parts of `ob` with `f` */
  def mapParts[A](ob:Ob,f:Part => A): Map[Part,A] =
    getParts(ob).map(part => part -> f(part)).toMap

  /* Filter the parts of `ob` according to `pred` */
  def filterParts[A](ob:Ob,pred:Part => Boolean): Seq[Part] =
    getParts(ob).filter(pred)

  /* Collect any parts in the ACSet with UID `id0` */
  def collectParts(id0:UID): Seq[Part] = for 
    ob <- schema.obSeq
    part <- getParts(ob)
    if part.id == id0
  yield part


  /* Get an optional part with UID `id0` */
  def tryPart(id0:UID): Option[Part] = collectParts(id0).headOption

  /* Get a part with UID `id0` (unsafe) */
  def getPart(id0:UID): Part = collectParts(id0).head 


  /** Setting **/

  /* Setting aggregate data/props */
  
  /* Reset the data associated with `part` to `data` */
  def resetData(part:Part,data:D) = this.copy(
    partStore = partStore.updated(part.ob.id,
      partStore(part.ob.id).resetData(part.id,data)
    )
  )
      
  /* Merge `data` into the existing data for `part` */
  def mergeData(part:Part,data:D) = resetData(part,getData(part).merge(data)) 



  /* Set properties `props` for `part` */
  def setProps(part:Part,props:PropMap) = resetData(part,getData(part).setProps(props)) 

  /* Set the properties of each `Part` with the associated `PropMap` */
  def setProps(kvs:Iterable[(Part,PropMap)]): ACSet[D] = 
    def helper(acset:ACSet[D],ob:Ob,kvs:Iterable[(UID,PropMap)]) = acset.copy(
      partStore = acset.partStore.updated(ob.id,
        acset.partStore(ob.id).setProps(kvs)
      )
    )
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => helper(acset,ob,kvs.map((part,props) => (part.id,props)))
    }

  def setProps(ob:Ob,props:PropMap): ACSet[D] = setProps(getParts(ob).map(_ -> props))

  /* Merge the existing data for `part` into `data` */
  def softMergeData(part:Part,data:D) = resetData(part,getData(part).softMerge(data)) 

  /* Set properties `props` for `part`, if unset */
  def softSetProps(part:Part,props:PropMap) = resetData(part,getData(part).softSetProps(props)) 


  /* Set the properties of each `Part` from the associated `PropMap` if they are unset */
  def softSetProps(kvs:Iterable[(Part,PropMap)]): ACSet[D] = 
    def helper(acset:ACSet[D],ob:Ob,kvs:Iterable[(UID,PropMap)]) = acset.copy(
      partStore = acset.partStore.updated(ob.id,
        acset.partStore(ob.id).softSetProps(kvs)
      )
    )
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => helper(acset,ob,kvs.map((part,props) => (part.id,props)))
    }

  def softSetProps(ob:Ob,props:PropMap): ACSet[D] = softSetProps(getParts(ob).map(_ -> props))


  /* Setting individual properties */


  /* Set the value for `f` at `part` to `v` */
  def setProp(f:Property,part:Part,v:f.Value) = 
    resetData(part,getData(part).setProp(f,v))

  /* Set the value for `f` with the given part-value pairs */
  def setProp(f:Property,kvs:Iterable[(Part,f.Value)]): ACSet[D] =
    def helper(acset:ACSet[D],f:Property,ob:Ob,kvs:Iterable[(UID,f.Value)]) = acset.copy(
      partStore = acset.partStore.updated(ob.id,
        acset.partStore(ob.id).setProp(f,kvs)
      )
    )
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,obkvs)) => helper(acset,f,ob,obkvs.mapKeys(_.id))
    }

  /* Set the value for `f` to `v` for all parts in `ob` */
  def setProp(f:Property,ob:Ob,v:f.Value): ACSet[D] =
    setProp(f,getParts(ob).map(_ -> v))
  

  /* Set the value for `f` at `part` to `v`, if unset */
  def softSetProp(f:Property,part:Part,v:f.Value) = 
    if hasProp(f,part) then this else setProp(f,part,v)

  /* Set the value for `f` with the given part-value pairs, if unset */
  def softSetProp(f:Property,kvs:Iterable[(Part,f.Value)]): ACSet[D] =
    def helper(acset:ACSet[D],f:Property,ob:Ob,kvs:Iterable[(UID,f.Value)]) = acset.copy(
      partStore = acset.partStore.updated(ob.id,
        acset.partStore(ob.id).softSetProp(f,kvs)
      )
    )
    
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => helper(acset,f,ob,kvs.map((part,v) => (part.id,v)))
    }
    
    
  /* Set the value for `f` to `v` for  parts in `ob` if missing */
  def softSetProp(f:Property,ob:Ob,v:f.Value): ACSet[D] =
    softSetProp(f,getParts(ob).map(_ -> v))

  /* Set the value for `f` to `v` from `parts` if missing */
  def softSetProp(f:Property,parts:Iterable[Part],v:f.Value): ACSet[D] =
    softSetProp(f,parts.map(_ -> v))
  


  /* Removing properties */


  /* Remove the properties `fs` from `part` */
  def remProps(part:Part,fs:Iterable[Property]) = resetData(part,getData(part).remProps(fs))

  /* Remove the properties `fs` from all parts in `ob` */
  def remProps(fs:Iterable[Property],ob:Ob): ACSet[D] = this.copy(
    partStore = partStore.updatedWith(ob.id)(opt =>
      opt.map(partset => partset.copy(
        dataStore = partset.dataStore.map( (id,data) =>
          id -> data.remProps(fs)  
        )
      ))
    )
  )

  /* Remove `f` from all parts in `ob` */
  def remProp(f:Property,ob:Ob): ACSet[D] = remProps(Seq(f),ob)

  /* Remove properties from all parts in the associated objects */
  def remProps(kvs:Iterable[(Property,Ob)]): ACSet[D] = kvs.toSeq match
    case Seq() => this
    case (f,ob) +: tail => remProp(f,ob).remProps(tail)
  
  /* Remove `f` from `part` */
  def remProp(f:Property,part:Part): ACSet[D] = remProps(part,Seq(f))


  
  /** Adding parts **/

  def addPartsById(ob:Ob,partData:Iterable[(UID,D)]): (ACSet[D],Iterable[Part]) = 
    /* Collect any schema elements referred to in `partData` */
    val schemaElts: Seq[Elt] = ob +: partData.toSeq.flatMap(
      (_,data) => data.generators().values
    )
    /* Construct parts with the given ids */
    val newParts = partData.map((id,_) => Part(id,ob))
    if schema.hasElts(schemaElts)
    then
      /* No new schema elements */
      val next = this.copy(
        partStore = partStore + (ob.id -> partStore(ob.id).addParts(partData))
      )
      (next,newParts)
    
    else schema match
      /* If `schema` is dynamic, add new elements */
      case schema:DynamicSchema => (
        this.copy(
          schema = schema ++ schemaElts,
          partStore = partStore + (ob.id -> partStore(ob.id).addParts(partData))
        ),
        newParts
      )
      /* If `schema` is static, warn and abort */
      case _ =>
        val badElts = schemaElts diff schema.elts.mkString(",")
        println(s"Attempted to add elements $badElts to StaticSchema $schema")
        (this,Seq())

  def addPartById(ob:Ob,id:UID,data:D): (ACSet[D],Part) =
    val (next,parts) = addPartsById(ob,Seq(id -> data))
    (next,parts.head)

  /* Add a part to `ob` initialized with `data` */
  def addPart(ob:Ob,data:D): (ACSet[D],Part) = 
    val (next,parts) = addParts(ob,Seq(data))
    (next,parts.head)

  /* Add a collection of parts to `ob` initialized with `ds` */
  def addParts(ob:Ob,ds:Iterable[D]): (ACSet[D],Iterable[Part]) = 
    addPartsById(ob,ds.map(UID(ob.toString + "@") -> _))
  


  /** Removing parts **/

  /* The fiber over a part `p` is described by X:Ob => f:Hom(X,p.ob) => f⁻¹(p) */
  type FiberMap = Map[Hom[_,_],Iterable[Part]]
  object FiberMap:
    def apply() = Map[Hom[_,_],Iterable[Part]]()

  def mergeFibers(m1:FiberMap,m2:FiberMap): FiberMap = 
    m1.merge(_ ++ _)(m2)

  def collectFibers(m:FiberMap): Map[Ob,Seq[Part]] = m.foldLeft(Map[Ob,Seq[Part]]()){
    case (coll,(f,parts)) => coll + (f.dom -> (coll.get(f.dom).getOrElse(Seq()) ++ parts))
  }

  /* Collect the fiber of `f` over `part` */
  def fiberOf(f:Hom[_,_],part:Part): Seq[Part] = if f.codom != part.ob
    then Seq()
    else filterProp(f,_ == part).keys.toSeq
    
  /* Collect all fibers over `part` */
  def fibersOf(part:Part): FiberMap = schema.homSeq
    .filter(_.codom == part.ob)
    .map(f => f -> fiberOf(f,part))
    .toMap

  /* Remove a collection of `parts`, with optional `cascade` */
  def remParts(parts:Iterable[Part],cascade:Boolean = true): ACSet[D] = 
    val toRemove: Map[Ob,Seq[Part]] = 
      parts.map(p => p.ob -> Seq(p)).toMap.merge(_ ++ _)(
        if cascade
        then collectFibers(
          parts.map(fibersOf).foldLeft(FiberMap())(mergeFibers)        
        )
        else Map()
      )
    this.copy(
      partStore = toRemove.foldLeft(partStore){
        case (store,(ob,parts)) =>
          store.updated(ob.id, store(ob.id).remParts(parts.map(_.id)))
      }
    )


  /* Remove a single `part`, with optional `cascade` */
  def remPart(part:Part,cascade:Boolean = true): ACSet[D] = 
    remParts(Seq(part),cascade) 
    
  
  /* Remove `obs` from the schema, with optional `cascade` */
  def remObs(obs:Iterable[Ob],cascade:Boolean = true) = schema match
    case schema:DynamicSchema =>  
      remParts(obs.flatMap(getParts),cascade).copy(
        schema = schema.remElts(obs.map(_.id),cascade)
      )
    case _ =>
      println(s"Warning: attempted to remove objects $obs from static schema $schema")
      remParts(obs.flatMap(getParts),cascade)

  /* Remove `fs` from the schema, with optional `cascade` */
  def remHoms(fs:Iterable[Hom[_,_]],cascade:Boolean = true) = schema match
    case schema:DynamicSchema =>  
      remProps(fs.map(f => f -> f.dom)).copy(
        schema = schema.remElts(fs.map(_.id),cascade)
      )
    case _ =>
      println(s"Warning: attempted to remove homs $fs from static schema $schema")
      remProps(fs.map(f => f -> f.dom))

  /* Remove `elts` from the schema, with optional `cascade` */
  def remElts(elts:Iterable[Elt],cascade:Boolean = true) = 
    remObs(elts.collect{ case ob:Ob => ob })
      .remHoms(elts.collect{ case hom:Hom[_,_] => hom })


  /* Add `elts` from the schema */
  def addElts(elts:Iterable[Elt]) = schema match
    case schema:DynamicSchema => this.copy(
      schema = schema.addElts(elts)
    )
    case _ =>
      println(s"Warning: attempted to add schema elements $elts to static schema $schema")
      this

  /* Remove an `elt` from the schema */
  def -(elt:Elt) = remElts(Seq(elt))

  /* Remove `elts` from the schema */
  def --(elts:Iterable[Elt]) = remElts(elts)

  /* Add an `elt` to the schema */
  def +(elt:Elt) = addElts(Seq(elt))

  /* Add `elts` to the schema */
  def ++(elts:Iterable[Elt]) = addElts(elts)
  
  /* Setting order & properties */

  /* Move `p` to the front of the `ids` ordering */
  def moveToFront(p:Part) = this.copy(
    partStore = partStore.updated(p.ob.id,partStore(p.ob.id).moveToFront(p.id))
  )




object ACSet:

  /** Construct a new ACSet with schema `s` and top-level parts `props` */
  def apply[D:PartData](s: Schema): ACSet[D] = 
    new ACSet("",s,PropMap(),PartStore())


