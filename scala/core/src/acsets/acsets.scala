package semagrams.acsets



import semagrams._
import semagrams.util._

import upickle.default._
import semagrams.acsets._
import semagrams.state._




case class ACSet[D:PartData](
  name: String,
  schema:Schema,
  globalProps: PropMap,
  partStore: PartStore[D]
):
  // override def toString = if name == ""
  //   then s"ACSet($schema)"
  //   else name

  def getParts(ob:Ob): Seq[Part] = partStore(ob.id).ids.map(Part(_,ob))

  def getDataSeq(ob:Ob): Seq[(Part,D)] = getParts(ob).map(part => part -> partStore(ob.id).getData(part.id))
  def getPropSeq(ob:Ob) = getDataSeq(ob).map((part,data) => part -> data.getProps())  

  def getData(ob:Ob) = getDataSeq(ob).toMap
  def getProps(ob:Ob) = getPropSeq(ob).toMap
  
  def getData(part:Part): D = partStore(part.ob.id).dataStore.get(part.id)
    .getOrElse(PartData[D]())
  def getProps(part:Part): PropMap = getData(part).getProps()

  def collectDataSeq(f:Property,ob:Ob) = partStore(ob.id).dataStore.collect{
    case (id,data) if data.contains(f) => Part(id,ob) -> data.getProp(f)
  }
  def collectData(f:Property,ob:Ob) = collectDataSeq(f,ob).toMap

  def tryProp(f:Property,part:Part) = partStore(part.ob.id).dataStore(part.id).tryProp(f)
  def getProp(f:Property,part:Part) = tryProp(f,part).get
  def hasProp(f:Property,part:Part) = tryProp(f,part).isDefined
  
  def tryProp(f:Property,ob:Ob): Map[Part,Option[f.Value]] = getParts(ob).map(part => part -> tryProp(f,part)).toMap
  def collectProp(f:Property,ob:Ob) = tryProp(f,ob).collect{ case part -> Some(v) => part -> v }
  def getProp(f:Property,ob:Ob) = tryProp(f,ob).map((part,opt) => (part,opt.get))
  def hasProp(f:Property,ob:Ob) = tryProp(f,ob).map((part,opt) => (part,opt.isDefined))
  
  def getHom(f:Hom[_,_]): Map[Part,f.Value] = if schema.hasHom(f) then getProp(f,f.dom) else Map()
  def collectHom(f:Hom[_,_]): Map[Part,f.Value] = if schema.hasHom(f) then collectProp(f,f.dom) else Map()

  def collectParts(id0:UUID): Seq[Part] = for 
    ob <- schema.obSeq
    part <- getParts(ob)
    if part.id == id0
  yield part

  def tryPart(id0:UUID): Option[Part] = collectParts(id0).headOption
  def getPart(id0:UUID): Part = collectParts(id0).head 


  def resetData(part:Part,data:D) = this.copy(
    partStore = partStore.updated(part.ob.id,
      partStore(part.ob.id).resetData(part.id,data)
    )
  )
      
  def mergeData(part:Part,data:D) = resetData(part,getData(part).merge(data)) 
  def softMergeData(part:Part,data:D) = resetData(part,getData(part).softMerge(data)) 

  def setProps(part:Part,props:PropMap) = resetData(part,getData(part).setProps(props)) 
  def softSetProps(part:Part,props:PropMap) = resetData(part,getData(part).softSetProps(props)) 
  def remProps(part:Part,props:Iterable[Property]) = resetData(part,getData(part).remProps(props))

  def setProp(f:Property,part:Part,v:f.Value) = setProps(part,PropMap(f -> v))
  def softSetProp(f:Property,part:Part,v:f.Value) = 
    if hasProp(f,part) then this else setProp(f,part,v)

  def setProp(f:Property,ob:Ob,kvs:Iterable[(UUID,f.Value)]) = this.copy(
    partStore = partStore.updated(ob.id,
      partStore(ob.id).setProp(f,kvs)
    )
  )
  def setProp(f:Property,kvs:Iterable[(Part,f.Value)]): ACSet[D] =
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => acset.setProp(f,ob,kvs.map((part,v) => (part.id,v)))
    }
  
  def setProps(ob:Ob,kvs:Iterable[(UUID,PropMap)]) = this.copy(
    partStore = partStore.updated(ob.id,
      partStore(ob.id).setProps(kvs)
    )
  )
  def setProps(kvs:Iterable[(Part,PropMap)]): ACSet[D] = 
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => acset.setProps(ob,kvs.map((part,props) => (part.id,props)))
    }


  def softSetProp(f:Property,ob:Ob,kvs:Iterable[(UUID,f.Value)]) = this.copy(
    partStore = partStore.updated(ob.id,
      partStore(ob.id).softSetProp(f,kvs)
    )
  )
  def softSetProp(f:Property,kvs:Iterable[(Part,f.Value)]): ACSet[D] =
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => acset.softSetProp(f,ob,kvs.map((part,v) => (part.id,v)))
    }
  
  def softSetProps(ob:Ob,kvs:Iterable[(UUID,PropMap)]) = this.copy(
    partStore = partStore.updated(ob.id,
      partStore(ob.id).softSetProps(kvs)
    )
  )
  def softSetProps(kvs:Iterable[(Part,PropMap)]): ACSet[D] = 
    kvs.groupBy(_._1.ob).foldLeft(this){
      case (acset,(ob,kvs)) => acset.softSetProps(ob,kvs.map((part,props) => (part.id,props)))
    }
  // def softSetProp(f:Property,part:Part,v:f.Value) = 
  //   if hasProp(f,part) then this else setProp(f,part,v)


  def remProp(f:Property,part:Part) = remProps(part,Seq(f))

  def addPartsById(ob:Ob,partData:Iterable[(UUID,D)]): (ACSet[D],Iterable[Part]) = 
    val schemaElts: Seq[Elt] = ob +: partData.toSeq.flatMap(
      (_,data) => data.generators().values
    )
    val newParts = partData.map((id,_) => Part(id,ob))
    if schema.hasElts(schemaElts)
    then (
      this.copy(
        partStore = partStore + (ob.id -> partStore(ob.id).addParts(partData))
      ),
      newParts
    )
    else schema match
      case schema:DynamicSchema => (
        this.copy(
          schema = schema ++ schemaElts,
          partStore = partStore + (ob.id -> partStore(ob.id).addParts(partData))
        ),
        newParts
      )
      case _ =>
        val badElts = schemaElts diff schema.elts.mkString(",")
        println(s"Attempted to add elements $badElts to StaticSchema $schema")
        (this,Seq())


  def addPart(ob:Ob,data:D): (ACSet[D],Part) = 
    val (next,parts) = addParts(ob,Seq(data))
    (next,parts.head)

  def addPart(part:Part,data:D): ACSet[D] =
    addParts(Seq(part -> data))
    
  def addParts(ob:Ob,ds:Iterable[D]): (ACSet[D],Iterable[Part]) = 
    addPartsById(ob,ds.map(UUID(ob.toString + "-Part") -> _))

  def addParts(parts:Iterable[(Part,D)]): ACSet[D] = 
    parts.groupBy(_._1.ob).foldLeft(this){ 
      case (acset,ob -> partData) => 
        val pd = partData.map((part,data) => (part.id,data))
        acset.addPartsById(ob,pd)._1
    } 

  
  type FiberMap = Map[Ob,Map[_<:Hom[_,_],Seq[Part]]]
  def fiberOf(part:Part): FiberMap = schema.obSeq
    .collect{ case ob if partStore.contains(ob.id) =>
      ob -> schema.arrowSeq.filter(_.codom == ob).map(f =>
        f -> collectHom(f).collect{ case p0 -> p1 if p1 == part => p0 }.toSeq
      ).toMap
    }.toMap

  def fiberOf(id0:UUID): FiberMap = tryPart(id0) match
    case Some(part) => fiberOf(part)
    case None => Map()
  
  

  def remPart(ob:Ob,id0:UUID,cascade:Boolean): ACSet[D] = if cascade
    then 
      val toRemove: Map[Ob,Seq[UUID]] = fiberOf(id0).map((x,arrowMap) =>
        x -> (arrowMap.values.toSeq.flatten.map(_.id) 
          ++ (if x == ob then Seq(id0) else Seq())
        )
      )
      this.copy(
        partStore = toRemove.foldLeft(partStore){
          case (store,(ob,ids)) => 
            partStore.updated(ob.id, partStore(ob.id).remParts(ids))
        }
      )
    else this.copy(
      partStore = partStore.updated(ob.id,partStore(ob.id).remPart(id0))
    )

  def remPart(part:Part,cascade:Boolean = true): ACSet[D] = remPart(part.ob,part.id,cascade) 
    
  /* Setting order & properties */
  def moveToFront(p:Part) = this.copy(
    partStore = partStore.updated(p.ob.id,partStore(p.ob.id).moveToFront(p.id))
  )

  // def moveToIndex(p:Part,j:Int) = 
  //   val ps = partStore(p.ob.id).moveToIndex(p.id,j)
  //   println(s"ps = $ps")
  //   this.copy(
  //   partStore = partStore + (p.ob.id -> partStore(p.ob.id).moveToIndex(p.id,j))
  // )
  /* Global properties */
  // def hasProp(f:Property) = globalProps.contains(f)
  // def tryProp(f:Property) = globalProps.get(f)
  // def getProp(f:Property) = globalProps(f)
  // def setProp(f:Property,v:f.Value) = 
  //   setProps(PropMap(f -> v))
  // def softSetProp(f:Property,v:f.Value) =
  //   softSetProps(PropMap(f -> v))
  // def remProp(f:Property) =
  //   remProps(Seq(f))

  // def +(f:Property,v:f.Value) = setProp(f,v)
  // def -(f:Property) = remProp(f) 

  // def hasProps(fs:Iterable[Property]) = fs.forall(hasProp)
  // def tryProps(fs:Iterable[Property]) = fs.map(tryProp)
  // def resetProps(props:PropMap) = this.copy(
  //   schema = schema -- globalProps.keySeq ++ props.keySeq,
  //   globalProps = props
  // )
  // def setProps(props:PropMap) = this.copy(
  //   schema = schema ++ props.keySeq,
  //   globalProps = globalProps ++ props
  // )
  // def softSetProps(props:PropMap) = 
  //   setProps(props ++ globalProps)

  // def remProps(props:Iterable[Property]) =
  //   resetProps(globalProps -- props)

  // def ++(props:PropMap) = setProps(props)
  // def --(props:Iterable[Property]) = remProps(props)


  /* Manipulating parts */

  // def getData(part:Part): D


  // //   def _addParts(data:Seq[(Part,Data)]): (A,Seq[Part])  
  // //   def remParts(ps:Seq[Part]): A
  // //   def moveToIndex(p:Part,i:Int): A
  // //   def setData(kvs:Seq[(Part,Data)]): A

  // //   def addSchemaElt(elt:Elt): A = a.addSchemaElts(Seq(elt))
  // //   def +(elt:Elt) = a.addSchemaElt(elt)
  // //   def ++(elts:Seq[Elt]) = a.addSchemaElts(elts)
  // //   def ++(s:Sch) = a.addSchemaElts(s.generators)


  // // def apply(f:Property) = getProp(f)


  //       def addSchemaElts(elts:Seq[Elt]) = a.copy(
  //         schema = a.schema ++ elts
  //       )

  //       def remSchemaElts(ids:Seq[UUID]) = 
  //         val newSchema = a.schema.remIds(ids)
  //         val newParts = 
  //           (a.partStore -- ids.flatMap(a.schema.tryOb))
  //             .map((ob,parts) => ob -> parts.copy(
  //               propStore = a.partStore(ob).propStore.map( (id,data) =>
  //                 id -> data.filter{ (f,fval) => fval match
  //                   case part:Part =>
  //                     val partids = part.ob.generators.map(_.id)
  //                     if (ids intersect partids).isEmpty
  //                     then true
  //                     else false
  //                   case _ => true
  //                 }
  //               )
  //             ))


  //         a.copy(
  //           schema = newSchema,
  //           partStore = newParts
  //         )

  //       def replaceSchemaElts(elts:Seq[(UUID,Elt)]) =
  //         val newSchema = a.schema.remIds(elts.map(_._1))
  //           .addElts(elts.map(_._2))

  //         val newParts = a.partStore.map{ (ob,partset) =>
  //           val newOb = ob match
  //             case ob:GenOb => elts.find(_._1 == ob.id) match
  //               case Some((id,newOb:Ob)) => newOb
  //               case _ => ob
  //             case _ => ob
            
  //           val newProps = a.partStore(ob).propStore.map((id0,data) =>
  //             id0 -> data.filter((f,fval) => fval match
  //               case part:Part if part.ob.generators.map(_.id).contains(id0) =>
  //                 false
  //               case _ => true
  //             )
  //           )

  //           newOb -> a.partStore(ob).copy(
  //             propStore = newProps
  //           )
  //         }


  //         a.copy(
  //           schema = newSchema,
  //           partStore = newParts
  //         )


  //       def setSchema(newSch: S) = a.copy(
  //         schema = newSch,
  //         partStore = a.partStore.toSeq.flatMap {
  //           case (ob:GenOb,partset) => newSch.obs.find(_.id == ob.id)
  //             .map(newOb => newOb -> partset)
  //           case _ => None
  //         }.toMap
  //       )


  // override def toString = name


    // def globalData: Data
    // def setGlobalProp(f:Property,v:f.Value): A
    // def remGlobalProp(f:Property): A
    // def addSchemaElts(elts:Seq[Elt]): A
    // def remSchemaElts(ids:Seq[UUID]): A
    // def replaceSchemaElts(elts:Seq[(UUID,Elt)]): A

    // def setSchema(newSchema:Sch): A
    
    /* Manipulating parts */

    /* Manipulating parts */

    
    /* Generic methods */

    // def renameSchemaElt(elt:SchemaElt,newName:String) = 
      
    //   a.setSchema(
    //   a.schema.renameElt(elt.id,newName)
    // )
    // def renameSchemaElt(id:UUID,newName:String) = a.setSchema(
    //   a.schema.renameElt(id,newName)
    // ) 
      


    /* Global properties */
//     def globalProps: PropMap = a.globalData.getProps()
//     def tryGlobalProp(f:Property): Option[f.Value] = a.globalProps.get(f)
//     def getGlobalProp(f:Property): f.Value = a.globalProps(f)



//     def setGlobalProps(props:PropMap): A = props.pmap.keys.toSeq match
//       case Seq() => a
//       case f +: rest => 
//         a.setGlobalProp(f,props(f)).setGlobalProps(props - f)
        
//     def softSetGlobalProps(props:PropMap): A =
//       a.setGlobalProps(props ++ globalProps)

//     /* Manipulating parts */

//     // def getParts(ob:Ob): Seq[Part] = ob match
//     //   case ob:ACSetOb => _getParts(ob)
//     //   case _ => Seq()
//     def allParts() = a.schema.obs
//       .flatMap(a.getParts(_))

//     def allProps() = a.schema.obs
//       .flatMap(a.getProps(_))

//     def selectedProps() = a.allProps()
//       .filter((_,props) => props.contains(Selected))

//     def selected() = a.selectedProps().map(_._1) 

//     def hasPart(part:Part) =
//         a.getParts(part.ob).contains(part)

//     def addParts(ob:Ob,data:Seq[Data | PropMap]): (A,Seq[Part]) = 
//       a._addParts(data.map{ _ match
//         case props:PropMap => Part(ob) -> PartData[Data](props)
//         case d:Data => Part(ob) -> d
//         // case (part:Part,props:PropMap) => part -> PartData[Data](props)
//         // case (part:Part,d:Data) => part -> d
//       })
    
//     def addParts(kvs:Seq[(Part,Data | PropMap)]): (A,Seq[Part]) =
//       a._addParts(kvs.map((part,data) => data match
//         case props:PropMap => part -> PartData[Data](props)
//         case d:Data => part -> d  
//       ))
 
//     def addPart(part:Part,data:Data | PropMap): (A,Part) =
//       val (acset,ps) = a.addParts(Seq(part -> data))
      
//       (acset,ps.head)


//     def addPart(ob:Ob,data:Data | PropMap = PropMap()): (A,Part) =
//       val (acset,ps) = a.addParts(ob,Seq(data))
//       (acset,ps.head)

//     def addParts(ob:Ob,n:Int,data: Data | PropMap = PropMap()): (A,Seq[Part]) =
//       a.addParts(ob,Seq.fill(n)(data))

      
//     // def remParts(ps:Seq[Part]) = a.remParts(ps.collect{
//     //   case p:Part => p
//     // })
//     def remPart(p:Part) = a.remParts(Seq(p))
    

    // def moveToEnd(p:Part): A = 
    //   moveToIndex(p,a.getParts(p.ob).length - 1)


//     /* PartData getters & setters */

//     // def getData(p:Part): Data = p match
//     //   case p:Part => a._getData(p)
//     //   case _ => data()

//     def getData(ps:Seq[Part]): Map[Part,Data] =
//       ps.map(p => p -> a.getData(p)).toMap

//     def getData(ob:Ob): Map[Part,Data] =
//       a.getData(a.getParts(ob))
      
//     def getDataSeq(ps:Seq[Part]): Seq[(Part,Data)] =
//       ps.map(p => p -> a.getData(p))

//     def getDataSeq(ob:Ob): Seq[(Part,Data)] =
//       a.getDataSeq(a.getParts(ob))
      

//     // def setData(kvs:Seq[(Part,Any)]): A =
//     //   _setData(kvs.collect{ case (p:Part,d:Data) => (p,d)})

//     def setData(p:Part,data:Data): A = setData(Seq(p -> data))


//     /* Property getters */
    
//     /* Get a family of properties for each part */
//     def getProps(part:Part): PropMap =
//       a.getData(part).getProps()

//     def getProps(ps:Seq[Part]): Map[Part,PropMap] =
//       ps.map(p => p -> a.getProps(p)).toMap

//     def getProps(ob:Ob): Map[Part,PropMap] =
//       a.getProps(a.getParts(ob))


//     /* Check if an individual property is defined */
//     def hasProp(f:Property,part:Part): Boolean =
//       hasPart(part) & getProps(part).contains(f)
    
    
//     /* Check if a family of properties is defined */
//     def hasProps(fs:Seq[Property],part:Part): Boolean =
//       fs.forall(hasProp(_,part))
//     def hasProps(props:PropMap,part:Part): Boolean =
//       hasProps(props.keySeq,part)

    


//     def tryProp(f:Property,part:Part): Option[f.Value] =
//       a.getProps(part).get(f)

//     def tryProp(f:Property,ps:Seq[Part]): Map[Part,Option[f.Value]] =
//       ps.map(p =>
//         p -> a.tryProp(f,p)    
//       ).toMap

//     def tryProp(f:Property,ob:Ob): Map[Part,Option[f.Value]] =
//       a.tryProp(f,getParts(ob))

//     def collectProp(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
//       ps.filter(hasProp(f,_)).map(p =>
//         p -> a.getProp(f,p)    
//       ).toMap

//     def collectProp(f:Property,ob:Ob): Map[Part,f.Value] =
//       a.collectProp(f,getParts(ob))


//     def getProp(f:Property,p:Part): f.Value =
//       tryProp(f,p).get

//     def getProp(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
//       ps.map(p => p -> a.getProp(f,p)).toMap

//     def getProp(f:Property,ob:Ob): Map[Part,f.Value] =
//       a.getProp(f,a.getParts(ob))

//     def getProp(f:Property,p:Part,default:f.Value): f.Value =
//       tryProp(f,p).getOrElse(default)

//     def getProp(f:Property,ps:Seq[Part],default:f.Value): Map[Part,f.Value] =
//       ps.map(p => p -> a.getProp(f,p,default)).toMap

//     def getProp(f:Property,ob:Ob,default:f.Value): Map[Part,f.Value] =
//       a.getProp(f,a.getParts(ob),default)


//     /* Property setters */

//     /* Set a single property on a single part */
//     def setProp(f:Property,p:Part,v:f.Value): A = 
//       a.setData(p,a.getData(p).setProp(f,v))

//     /* Set a single property on many parts */
//     def setProp(f:Property,kvs:Seq[(Part,f.Value)]): A =
//       a.setData(kvs.map( (part,fval) => 
//         part -> a.getData(part).setProp(f,fval)  
//       )) 
    
//     /* Set a single property on many parts */
//     def setProp(f:Property,ps:Seq[Part],v:f.Value): A =
//       a.setProp(f,ps.map(_ -> v))
      
//     /* Set a single property on many parts */
//     def setProp(f:Property,ob:Ob,v:f.Value): A =
//       a.setProp(f,a.getParts(ob),v)
      
//     /* Set many properties on a single part */
//     def setProps(p:Part,props:PropMap): A = 
//       a.setData(p,a.getData(p).setProps(props))
    
//     /* Set many properties on many parts */
//     def setProps(kvs:Seq[(Part,PropMap)]): A = 
//       a.setData(kvs.map((part,props) => part ->
//         a.getData(part).setProps(props)  
//       ))


//     /*=========================================*/
    
//     /* Set a single property on a single part if unset */
//     def softSetProp(f:Property,p:Part,v:f.Value): A = 
//       if a.hasProp(f,p) then a else a.setProp(f,p,v)
      
//     /* Set a single property on many parts if unset */
//     def softSetProp(f:Property,kvs:Seq[(Part,f.Value)]): A =
//       a.setProp(f,kvs.filterNot( (p,_) => a.hasProp(f,p) ))
      
//     /* Set many properties on a single part if unset */
//     def softSetProps(p:Part,props:PropMap): A = 
//       a.setProps(p,props ++ a.getProps(p))
      
//     /* Set many properties on many parts if unset */
//     def softSetProps(kvs:Seq[(Part,PropMap)]): A = 
//       a.setProps(kvs.map( (part,props) => 
//         part -> (props ++ a.getProps(part))
//       ))
    
//     def softSetObProps(kvs:Seq[(Ob,PropMap)]): A =
//       a.softSetProps(kvs.flatMap((ob,props) =>
//         a.getParts(ob).map(_ -> props)  
//       ))

//     def remProp(f:Property,p:Part): A = 
//       a.setData(p,a.getData(p).remProp(f))

//     def remProp(f:Property,ps:Seq[Part]): A =
//       a.setData(ps.map(p => p -> a.getData(p).remProp(f)))
      
//     def remProp(f:Property,ob:Ob): A =
//       a.remProp(f,a.getParts(ob))
      

//     def remProps(fs:Seq[Property],p:Part): A = 
//       a.setProps(p,a.getProps(p) -- fs)

  
//   }
// }



object ACSet:

  /** Construct a new ACSet with schema `s` and top-level parts `props` */
  def apply[D:PartData](s: Schema): ACSet[D] = 
    new ACSet("",s,PropMap(),PartStore())





//=================================================================




  //   /* Generic methods */

  //   /* Global properties */

  //   /* Manipulating parts */


  //   /* PartData getters & setters */



  // // type AACSet[SchOb<:AOb,Data,S<:AbstractSchema[SchOb],A<:AbstractACSet[SchOb,Data,S,A]] = AbstractACSet[SchOb,Data,S,A]

    
  //   /* ACSet */
  //   // type PartType
  //   // type Part <: Part[PartType]
  //   // type PartData
  //   // type SchemaType <: ASchema[SchemaType]

  //   /* Implementation interface */

  //   /* Abstract methods */

  //   /* Checks */

  //   /* Getters */

    // def collectProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
    //   ps.collect{ case p if hasProp(f,p) => p -> getProp(f,p) }.toMap
    // def collectProps(f:Property,ptype:PartType): Map[Part,f.Value] =
    //   collectProps(f,getParts(ptype))
    // def collectProps(f:Property,ob:schema.SchOb): Map[Part,f.Value] =
    //   collectProps(f,getParts(ob))

    // /** Mutation **/
    // /* Adding parts */
    // def addPart(ob:schema.SchOb,data:Seq[PartData]): (A,Part) =
    //   addPart(partType(ob),data)
    // def addParts(ptype:PartType,n:Int,data:PartData): (A,Seq[Part]) =
    //   addParts(ptype,Seq.fill(n)(data))
    // def addParts(ob:schema.SchOb,n:Int,data:PartData): (A,Seq[Part]) =
    //   addParts(partType(ob),n,data)
    // def addParts(ptype:PartType,n:Int,props:PropMap): (A,Seq[Part]) =
    //   addParts(ptype,n,partData(props))
    // def addParts(ob:schema.SchOb,n:Int,props:PropMap): (A,Seq[Part]) =
    //   addParts(partType(ob),n,partData(props))
    // def addParts(kvs:(PartType,PropMap) | (schema.SchOb,PropMap)
    //   | (PartType,PartData) | (schema.SchOb,PartData)*): (A,Seq[Part]) = 
    //   val cleankvs = kvs.map(_ match
    //     case (tp:PartType,data:PartData) => (tp,data)
    //     case (ob:schema.SchOb,data:PartData) => (partType(ob),data)
    //     case (tp:PartType,props:PropMap) => (tp,partData(props))
    //     case (ob:schema.SchOb,props:PropMap) => (partType(ob),partData(props))
    //   )
    //   cleankvs match
    //     case Seq() => (self,Seq())
    //     case (tp,data) +: tail =>  
    //       val (next,first) = addPart(tp,data)
    //       val (last,rest) = addParts(tail:_*)
    //       (last,first +: rest)

    // /* Removing parts */

    // def remPart(p:Part): A = remPart(this,p)  
    // def remParts(a:A,ps:Seq[a.Part]): A = ps match
    //   case Seq() => this
    //   case head +: tail => remPart(a,head).remParts(a,tail)
    // def remParts(ps:Seq[Part]): A = remParts(this,ps)
    // def remParts(ps:Seq[Part],pred:Part => Boolean): A =
    //   remParts(ps.filter(pred))
    // def remParts(ptype:PartType,pred:Part => Boolean): A =
    //   remParts(getParts(ptype),pred)
    // def remParts(ob:schema.SchOb,pred:Part => Boolean): A =
    //   remParts(getParts(ob),pred)

    
    // def issimple(data:PartData) =
    //   partData(getDataProps(data)) == data
    
    // def setData(part:Part,data:PartData): A = ?






sealed trait ACSetMsg[D:PartData] extends AtomicMessage[ACSet[D]]


case class AddPartMsg[D:PartData](ob:Ob,data:D,idOpt:Option[UUID] = None) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = idOpt match
    case Some(id) => a.addPart(Part(id,ob),data)
    case None => a.addPart(ob,data)._1

object AddPartMsg:
  def apply[D:PartData](ob:Ob,props:PropMap,id:UUID) =
    new AddPartMsg[D](ob,PartData(props),Some(id))

case class RemovePartMsg[D:PartData](part:Part) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = a.remPart(part)

case class ChangePropMsg[D:PartData](part:Part,pval:PropChange[_]) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = pval match
    case PropChange(f,_,Some(v)) => a.setProp(f,part,v)
    case PropChange(f,_,None) => a.remProp(f,part)

object ChangePropMsg:
  def apply[D:PartData](part:Part,f:Property,oldVal:f.Value,newVal:f.Value) = new ChangePropMsg(part,PropChange(f,oldVal,newVal))

case class SetPropsMsg[D:PartData](part:Part,props:PropMap) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = a.setProps(part,props)

case class SetPropMsg[D:PartData](part:Part,pval:PropVal[_]) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = pval match
    case PropVal(f,Some(v)) => a.setProp(f,part,v)
    case PropVal(f,None) => a.remProp(f,part)

object SetPropMsg:
  def apply[D:PartData](part:Part,f:Property,newVal:f.Value) = new SetPropMsg(part,PropVal(f,newVal))


// case class RenameOb[D:PartData](id:UUID,newOb:Ob) extends ACSetMsg[D]:
//   def execute(a:ACSet[D]) = 
//     a.replaceSchemaElts(Seq(id -> newOb))
//     // pval match
//     // case PropVal(f,Some(v)) => a.setProp(f,part,v)
//     // case PropVal(f,None) => a.remProp(f,part)

// object RenameOb:
//   def apply[D:PartData](id:UUID,ob:Ob) = new RenameOb[D](id,ob)



case class MsgSeq[D:PartData](override val msgs:Seq[ACSetMsg[D]]) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = msgs.foldLeft(a)((acset,msg) => msg.execute(acset))
  
object MsgSeq:
  def apply[D:PartData]() = new MsgSeq[D](Seq())
case class RemovePropMsg[D:PartData](prop:Property,part:Part) extends ACSetMsg[D]:
  def execute(a:ACSet[D]) = a.remProp(prop,part)









// trait AOb



// trait Schema[SchOb<:AOb]:
//   def obs: Seq[SchOb]

// trait ACSet[SchOb<:AOb,Data:PartData](val schema:Schema[SchOb]):
//   def parts(x:SchOb): Seq[Part]
//   def getData(p:Part): Data  

// case class Part(ob:SchOb,i:Int)

// case class SimpleOb(name:String) extends AOb
// case class BasicSchema(
//   obs: Seq[SimpleOb]
// ) extends Schema[SimpleOb]






  // override def toString = "SimpleACSet(" + "\n" + 
  //   schema.obs.map(ob => "\t" + ob.toString + "->\n" + 
  //     partStore(ob).propStore.toSeq.map((id,props) =>
  //       s"\t\t$props\n"
  //     ).mkString
  //   ).mkString + ")"

// object SimpleACSet:

//   import Table.TableDef

//   def schemaElts(defn:ACSetEltDef): Seq[SchemaElt] = defn match
//     case (t:TableDef,rows) => Seq(Table(t))
    
    
  

//   type ACSetEltDef = PartDef
//   type PartDef = (TableDef,Int) | (TableDef,Seq[PropMap])




  // def apply():SimpleACSet[BasicSchema] = apply[BasicSchema]()(simpleSchemaIsSchema)
  // def apply[S:Schema]():SimpleACSet[S] = apply(Schema[S]())


  // def apply(): SimpleACSet[BasicSchema] = 
  //   implicit val schDef: Schema[BasicSchema] = simpleSchemaIsSchema
  //   SimpleACSet(BasicSchema())
  // def apply[S:Schema](sch:S): SimpleACSet[S] = 
  //   SimpleACSet(sch,PropMap())
  // def apply[S:Schema](sch:S,props:PropMap): SimpleACSet[S] = 
  //   SimpleACSet(sch,props,PartStore())
  // def apply[S:Schema](defns:ACSetEltDef*): SimpleACSet[S] = defns match
  //   case Seq() => apply(Schema[S]())
  //   case defns => 
  //     val elts: Seq[SchemaElt] = defns.flatMap(schemaElts)
  //     val sch = Schema[S]().addElts(elts)
  //     apply(sch,defns:_*)


  // def apply[S:Schema](sch:S,defns:ACSetEltDef*): SimpleACSet[S] =
  //   implicit val acsetDef: ACSet[SimpleACSet[S]] = simpleACSetIsACSet[S]


  //   defns.foldLeft(
  //     new SimpleACSet[S](
  //       Schema[S](),
  //       PropMap(),
  //       PartStore()        
  //     )
  //   )( (acset,defn) => 
  //     defn match
  //     case (t:TableDef,rows) => rows match
  //       case n:Int =>
  //         acset.addParts(Table(t),n)._1
  //       case ps:Seq[PropMap] =>
  //         acset.addParts(Table(t),ps)._1
        
      
    

  //   )
    
  // def simpleACSetIsACSet[S:Schema](s:S): ACSetWithSchemaAndData[S,PropMap][SimpleACSet[S]] = 
  //   simpleACSetIsACSet[S]


  // def simpleACSetIsACSet[S](
  //   using sis:Schema[S],
  //   // did:PartData[D]
  // ):ACSetWithSchemaAndData[S,PropMap][SimpleACSet[S]] =
  //   new ACSet[SimpleACSet[S]] {

 

