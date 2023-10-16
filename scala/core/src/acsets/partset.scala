package semagrams.acsets


import semagrams._
import upickle.default._

/** An opaque wrapper around an integer */
case class Id(id: Int)
object Id {
  implicit val idRW: ReadWriter[Id] = readwriter[Int].bimap(
    _.id,
    Id(_)
  )
}



type PropStore = Map[Id,PropMap]
extension (pstore:PropStore)
  def remProp(id:Id,f:Property): PropStore =
    pstore + (id -> (pstore(id) - f))
  def addProp(id:Id,f:Property,v:f.Value) =
    pstore + (id -> (pstore(id).set(f,v)))






/**Storage class for the parts corresponding to an `Ob` in a schema.
  *
  * This is immutable; methods that logically mutate simply return new objects.
  *
  * @param nextId
  *   The id to assign to the next part added
  *
  * @param ids
  *   The ids of all the parts added so far. This is a `Seq` because we care
  *   about the ordering; when the ACSet is displayed this ordering is used when
  *   two sprites overlap.
  *
  * @param acsets
  *   The subacset corresponding to each id
  */
case class PartSet(
  nextId: Int,
  ids: Seq[Id],
  propStore: PropStore
) {

  /** Add multiple new parts, with subacsets given in `partacsets`.
    *
    * Returns a sequence of the ids of the added parts.
    */
  def addParts(parts: Seq[PropMap]): (PartSet, Seq[Id]) = {
    val newIds = nextId.to(nextId + parts.length - 1).map(Id.apply)
    val newParts = PartSet(
      nextId + parts.length,
      ids ++ newIds,
      propStore ++ newIds.zip(parts).toMap
    )
    (newParts, newIds)
  }

  /** Adds a single part with subacset `acs`, returns its id. */
  def addPart(props: PropMap): (PartSet, Id) = {
    val (p, newIds) = addParts(Seq(props))
    (p, newIds(0))
  }

  def remPart(id:Id): PartSet = remParts(Seq(id))

  def remParts(rem:Seq[Id]): PartSet = PartSet(
    nextId,
    ids.diff(rem),
    propStore -- rem
  )

    // case Seq() => this
    // case head +: tail => remPart(head).remParts(tail)
   
  
  def getProps(i: Id): PropMap =
    propStore.get(i).getOrElse(PropMap())
  def tryProp(f:Property,i:Id) =
    getProps(i).get(f)
  def getProp(f:Property,i:Id) = 
    getProps(i)(f)

  /** Set the subacset corresponding to `i` to `acs` */
  def setProps(i: Id, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> props)
    )
  }

  def setProp(i: Id, f:Property,v:f.Value): PartSet = {
    this.copy(
      propStore = propStore + (i -> propStore(i).set(f,v))
    )
  }

  def setProps(kvs:Seq[(Id,PropMap)]): PartSet = this.copy(
    propStore = propStore ++ kvs
  )


  

  def softSetProps(i: Id, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> (props ++ propStore(i)))
    )
  }

  def remProp(f:Property,i: Id) = {
    this.copy(
      propStore = propStore + (i -> (propStore(i) - f))
    )
  }

  def remProps(fs:Seq[Property],i: Id) = {
    this.copy(
      propStore = propStore + (i -> (propStore(i) -- fs))
    )
  }

  def remProps(kvs:Seq[(Property,Id)]): PartSet = kvs match
    case Seq() => this
    case (f,id) +: tail => remProp(f,id).remProps(tail)

  /** Move the id `i` to the front of the list of ids.
    *
    * This is used, for instance, when dragging a sprite so that the sprite goes
    * over the other parts.
    */
  def moveFront(i: Id) = {
    this.copy(
      ids = ids.filterNot(_ == i) :+ i
    )
  }

  def setData(kvs:Seq[(Id,PropMap)]) = this.copy(
    propStore = propStore ++ kvs
  )

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
    assert(ids.toSet == propStore.keySet)
    assert(n >= (0 +: ids.map(_.id)).max)
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




