package semagrams.acsets


import semagrams._
import semagrams.util._


import upickle.default._
import scala.annotation.targetName



/** A trait for the representation of individual parts in an ACSet, defining
 *  interoperability with `PropMap`s 
 */
trait PartData[Data]:
  def fromProps(props:PropMap): Data
  extension (d:Data)
    /* Implementation API */

    /* Get all available properties */
    def getProps(): PropMap

    /* Set a single property */
    def setProp(f:Property,v:f.Value): Data

    /* Remove a single property */
    def remProp(f:Property): Data


    /* Generic methods */

    /* Getting */

    /* Check for property `f` */
    def hasProp(f:Property): Boolean = d.getProps().contains(f)
    def contains(fs:Property*): Boolean = fs.forall(d.hasProp(_))
    def hasProps(fs:Iterable[Property]): Boolean = d.getProps().contains(fs.toSeq:_*)

    /* Return `Some(v)` if `f` is set and `None` otherwise */
    def tryProp(f:Property): Option[f.Value] = d.getProps().get(f)

    /* Return the assigned property value (unsafe) */
    def getProp(f:Property): f.Value = d.getProps()(f)    
    
    /* Setting */

    /* Set a single property (f -> v) */
    def +[T](kv:(Property{type Value = T},T)): Data = d.setProp(kv._1,kv._2)

    /* Set a family of properties `props` */
    def setProps(props:PropMap): Data =
      props.pmap.keys.toSeq match
        case Seq() => d
        case f +: rest => d.setProp(f,props(f)).setProps(props - f)

    /* Merge the properties of `d2` into `d`, overwriting on collisions */
    def merge[Data2:PartData](d2:Data2) = d.setProps(d2.getProps())

    /* Alias for `merge`. Overwrites on collisions */
    def ++[Data2:PartData](d2:Data2) = d.merge(d2)

    /* Remove a family of properties `props` */
    def remProps(props:Iterable[Property]): Data = props.toSeq match
      case Seq() => d
      case f +: rest => d.remProp(f).remProps(rest)
    

    /* Remove the properties in `d2` from those in `d`. */
    def diff[Data2:PartData](d2:Data2) = d.setProps(d2.getProps())


    /* Optionally set `f` to `v`, conditional on pred */
    def optionalSet(f:Property,pred:Option[f.Value] => Boolean,v:f.Value) =
      if pred(d.tryProp(f))
      then d.setProp(f,v)
      else d


    /* Optionally set `f` to `v`, conditional on pred or if the value is missing */
    def conditionalSet(f:Property,pred:f.Value => Boolean,v:f.Value) = d.tryProp(f) match
      case Some(v0) => if pred(v0) then d.setProp(f,v) else d
      case None => d.setProp(f,v)

    /* Set `f` to `v` if it is not set */
    def softSetProp(f:Property,v:f.Value): Data =
      d.conditionalSet(f,_=>false,v)

    /* Set the properties `props` if they are not set */
    def softSetProps(props:PropMap): Data =
      d.setProps(
        props.filterKeys(!hasProp(_))
      )
    
    def softMerge[Data2:PartData](d2:Data2) = d.softSetProps(d2.getProps())


    /* Collect all schema generators from `FKey` values */
    def generators() = d.getProps().keySeq.flatMap{
      case e:Elt => e.generators
      case _ => Seq()
    }.toMap


    
object PartData:
  
  def apply[D:PartData](props:PropMap = PropMap()) =
    val pd = summon[PartData[D]] 
    pd.fromProps(props)

  implicit val propsAreData:PartData[PropMap] = new PartData[PropMap] {
    def fromProps(props:PropMap) = props
    extension (props:PropMap)
      def getProps() = props
      def setProp(f:Property,v:f.Value) = props + (f,v)
      def remProp(f:Property) = props - f
      def merge(that:PropMap) = props ++ that
  }





type DataStore[D] = Map[UUID,D]
extension [D:PartData](dstore:DataStore[D])
  def remProp(id:UUID,f:Property) =
    dstore + (id -> (dstore(id).remProp(f)))
  def setProp(id:UUID,f:Property,v:f.Value) =
    dstore + (id -> (dstore(id).setProp(f,v)))






/**Storage class for the parts corresponding to an `Ob` in a schema.
  *
  * This is immutable; methods that logically mutate simply return new objects.
  *
  * @param ids
  *   The ids of all the parts added so far. This is a `Seq` because we care
  *   about the ordering; when the ACSet is displayed this ordering is used when
  *   two sprites overlap.
  *
  * @param dataStore
  *   The subacset corresponding to each id
  */
case class PartSet[D:PartData](
  ids: Seq[UUID],
  dataStore: DataStore[D]
) {

  /* Getting */ 

  /* Check if `parts` are contained in the `PartSet` */
  def hasParts(parts:Iterable[UUID]) = parts.forall(ids.contains)
  def hasPart(id:UUID) = ids.contains(id)

  def contains(id:UUID) = ids.contains(id)

  def contains(id:UUID,f:Property): Boolean = contains(id) &
    dataStore(id).contains(f)


  /* Get the data associated with a part (or empty) */
  def getData(i: UUID): D =
    dataStore.get(i).getOrElse(PartData())

  /* Get all properties for part `i` */
  def getProps(i: UUID): PropMap = getData(i).getProps()

  /* Return an optional value for `f` on part `i` */ 
  def tryProp(f:Property,i:UUID) =
    getProps(i).get(f)

  def hasProp(f:Property,i:UUID) =
    tryProp(f,i).isDefined

  /* Get the value for `f` for part `i` (unsafe) */ 
  def getProp(f:Property,i:UUID) = 
    getProps(i)(f)

  /* Setting */

  /* Individual properties */

  /* Set `f` for `i` to `v` */
  def setProp(f:Property,id: UUID,v:f.Value): PartSet[D] = 
    setProp(f,Seq(id -> v))

  def setProp(f:Property,kvs:Iterable[(UUID,f.Value)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((id,v) => (id -> dataStore(id).setProp(f,v)))
  )

  def setProps(id: UUID,props:PropMap): PartSet[D] = 
    setProps(Seq(id -> props))

  def setProps(kvs:Iterable[(UUID,PropMap)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((id,props) => id -> dataStore(id).setProps(props))
  )

  def softSetProp(f:Property,id: UUID,v:f.Value): PartSet[D] = 
    if hasProp(f,id) then this else setProp(f,id,v)

  def softSetProp(f:Property,kvs:Iterable[(UUID,f.Value)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((id,v) => (id -> dataStore(id).softSetProp(f,v)))
  )

  def softSetProps(id: UUID,props:PropMap): PartSet[D] = 
    softSetProps(Seq(id -> props))

  def softSetProps(kvs:Iterable[(UUID,PropMap)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((id,props) => id -> dataStore(id).softSetProps(props))
  )

  /* Set `f` for `i` to `v` */
  def remProp(f:Property,i: UUID): PartSet[D] = this.copy(
    dataStore = dataStore - i
  )

  /* _ + `f:i -> v` sets `f` to `v` for `i` */
  def +[T](assignment:PartVal[UUID,T]) =
    val PartVal(f,id,v) = assignment
    setProp(f,id,v)

  /* Remove the property `f` from `i` */ 
  def -(f:Property,i:UUID) = 
    remProp(f,i)

  /* Optionally set `f` to `v` on `i`, conditional on `pred` */
  def optionalSet(f:Property,i:UUID,pred:Option[f.Value] => Boolean,v:f.Value) = this.copy(
    dataStore = dataStore + (i -> dataStore(i).optionalSet(f,pred,v))
  )

  /* Conditionally set `f` to `v` on `i`, conditional on pred or if the value is missing */
  def conditionalSet(f:Property,i:UUID,pred:f.Value => Boolean,v:f.Value) = this.copy(
    dataStore = dataStore + (i -> dataStore(i).conditionalSet(f,pred,v))
  )
  
  /* Merging data */

  /* Merge the data in `kvs` into `dataStore` */
  def merge[Data2:PartData](kvs:Seq[(UUID,Data2)]) = this.copy(
    dataStore = dataStore ++ kvs.map((id,d2) => id -> dataStore(id).merge(d2))
  )

  def ++[Data2:PartData](kvs:Seq[(UUID,Data2)]) = merge(kvs)
  def ++[Data2:PartData](d2:Data2) = merge(ids.map(_ -> d2))

  def ++(f:Property,v:f.Value) = merge(ids.map(_ -> PropMap().set(f,v)))


  /** Merge unset data for  `i` with `d2` */
  def softMerge[Data2:PartData](kvs:Seq[(UUID,Data2)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((i,d2) => i -> dataStore(i).softMerge(d2))
  )

  /** Merge unset data for  `i` with `d2` */
  def softMerge[Data2:PartData](d2:Data2): PartSet[D] = 
    softMerge(ids.map(_ -> d2))(summon[PartData[Data2]])

  /** Overwrite the data for a sequence of `id -> data` pairs */  
  def resetData(kvs:Seq[(UUID,D)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs
  )
  /** Merge unset data for  `i` with `d2` */
  def resetData(id:UUID,d0:D): PartSet[D] = 
    resetData(Seq(id -> d0))

  
  
  def remProps(kvs:Seq[(UUID,Seq[Property])]) = this.copy(
    dataStore = dataStore ++ kvs.map((id,fs) => id -> dataStore(id).remProps(fs))
  )

  def --(kvs:Seq[(UUID,Seq[Property])]) = remProps(kvs)
  
  @targetName("remPropsAllIds")
  def --(props:Seq[Property]) = remProps(ids.map(_ -> props))

  /* Optionally set `f` for `i -> v` pairs, conditional on `pred` for the current value */
  def optionalSet(f:Property,pred:Option[f.Value] => Boolean,kvs:Seq[(UUID,f.Value)]) = this.copy(
    dataStore = dataStore ++ kvs.map((i,v) => i -> dataStore(i).optionalSet(f,pred,v))
  )
  def optionalSet(f:Property,pred:Option[f.Value] => Boolean,v:f.Value): PartSet[D] =
    optionalSet(f,pred,ids.map(_ -> v))

  /* Conditionally set `f` for `i -> v` pairs, conditional on `pred` or if the value is missing */
  def conditionalSet(f:Property,pred:f.Value => Boolean,kvs:Seq[(UUID,f.Value)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((i,v) => i -> dataStore(i).conditionalSet(f,pred,v))
  )
  def conditionalSet(f:Property,pred:f.Value => Boolean,v:f.Value): PartSet[D] =
    conditionalSet(f,pred,ids.map(_ -> v))
  

  /* Adding, moving & removing parts */

  /* Add a sequence of new parts with associated data */
  def addParts(parts: Iterable[(UUID,D)]): PartSet[D] = PartSet(
    parts.map(_._1).toSeq ++ ids,
    parts.toMap ++ dataStore
  )

  /** Adds a single part with a given `id` and `data` */
  def addPart(id:UUID,data: D): PartSet[D] =
    addParts(Seq(id -> data))

  /* Remove a sequence of parts with the ids `rem` */
  def remParts(rem:Iterable[UUID]): PartSet[D] = PartSet(
    ids.diff(rem.toSeq),
    dataStore -- rem
  )
   
  /** Remove a single part with a given `id` */
  def remPart(id:UUID): PartSet[D] = remParts(Seq(id))


  /* Move id `i` to the index `j` in the list of ids. Used to
   * change rendering order and ordering of ports
   */
  def moveToIndex(i: UUID, j: Int) = {
    val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
    this.copy(
      ids = (seg1 :+ i) ++ seg2
    )
  }


  /* Move id `i` to the front of `ids`. */
  def moveToEnd(i: UUID) = this.copy(
    ids = ids.filterNot(_ == i) :+ i
  )
  

  /* Move id `i` to the end of `ids`. */
  def moveToFront(i: UUID) = this.copy(
    ids = i +: ids.filterNot(_ == i)
  )
  

}
object PartSet:
  /* Validataion */
  def apply[D:PartData](ids:Seq[UUID],dataStore:DataStore[D]): PartSet[D] =
    assert(ids.toSet == dataStore.keySet)
    new PartSet(ids,dataStore.withDefaultValue(PartData()))
  
  /* Convenience methods */
  def apply[D:PartData](dataStore:DataStore[D]): PartSet[D] =
    apply(dataStore.keys.toSeq,dataStore)
  def apply[D:PartData](ids:Seq[UUID]): PartSet[D] =
    apply(ids,ids.map((_,PartData())).toMap)
  def apply[D:PartData](): PartSet[D] = PartSet(Seq(),Map())




  
case class Part(id:UUID,ob:Ob) extends Entity:

  val ty = ob

  override def toString = if ob.label == ""
    then "Part" + id.rand
    else ob.label + id.rand


  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.label + id.toString


object Part:
  def apply(x:Ob) = new Part(UUID("Part"),x)
  def apply(id:UUID,x:Ob) = new Part(id,x)
  
  
  def rw: ReadWriter[Part] = 
    readwriter[(UUID,UUID)].bimap[Part](
      part => (part.ob.id,part.id),
      (obId,partId) => Part(partId,Table(obId))
    )



type PartStore[D] = Map[UUID,PartSet[D]]

object PartStore:
  def apply[D:PartData]() = Map[UUID,PartSet[D]]().withDefaultValue(PartSet())

  implicit val rw: ReadWriter[Part] = readwriter[(UUID,UUID,String)].bimap[Part](
    part => (part.id,part.ob.id,part.ob.label),
    (partid,obid,obname) => Part(partid,Table(obid,obname))
  )


