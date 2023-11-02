package semagrams.acsets


import semagrams._
import semagrams.util._
import upickle.default._

/** An opaque wrapper around an integer */
// case class UUID(id: Int)
// object UUID {
//   implicit val idRW: ReadWriter[UUID] = readwriter[Int].bimap(
//     _.id,
//     UUID(_)
//   )
// }



type PropStore = Map[UUID,PropMap]
extension (pstore:PropStore)
  def remProp(id:UUID,f:Property): PropStore =
    pstore + (id -> (pstore(id) - f))
  def addProp(id:UUID,f:Property,v:f.Value) =
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
  // nextId: Int,
  ids: Seq[UUID],
  propStore: PropStore
) {

  /** Add multiple new parts, with subacsets given in `partacsets`.
    *
    * Returns a sequence of the ids of the added parts.
    */
  def addParts(parts: Seq[(UUID,PropMap)]): (PartSet, Seq[UUID]) = {
    val newIds = parts.map(_._1)
    val newParts = PartSet(
      newIds ++ ids,
      parts.toMap ++ propStore
    )
    (newParts, newIds)
  }

  /** Adds a single part with subacset `acs`, returns its id. */
  def addPart(id:UUID,props: PropMap): (PartSet, UUID) = {
    val (p, newIds) = addParts(Seq(id -> props))
    (p, newIds(0))
  }

  def remPart(id:UUID): PartSet = remParts(Seq(id))

  def remParts(rem:Seq[UUID]): PartSet = PartSet(
    // nextId,
    ids.diff(rem),
    propStore -- rem
  )

    // case Seq() => this
    // case head +: tail => remPart(head).remParts(tail)
   
  
  def getProps(i: UUID): PropMap =
    propStore.get(i).getOrElse(PropMap())
  def tryProp(f:Property,i:UUID) =
    getProps(i).get(f)
  def getProp(f:Property,i:UUID) = 
    getProps(i)(f)

  /** Set the subacset corresponding to `i` to `acs` */
  def setProps(i: UUID, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> props)
    )
  }

  def setProp(i: UUID, f:Property,v:f.Value): PartSet = {
    this.copy(
      propStore = propStore + (i -> propStore(i).set(f,v))
    )
  }

  def setProps(kvs:Seq[(UUID,PropMap)]): PartSet = this.copy(
    propStore = propStore ++ kvs
  )


  

  def softSetProps(i: UUID, props: PropMap): PartSet = {
    this.copy(
      propStore = propStore + (i -> (props ++ propStore(i)))
    )
  }

  def remProp(f:Property,i: UUID) = {
    this.copy(
      propStore = propStore + (i -> (propStore(i) - f))
    )
  }

  def remProps(fs:Seq[Property],i: UUID) = {
    this.copy(
      propStore = propStore + (i -> (propStore(i) -- fs))
    )
  }

  def remProps(kvs:Seq[(Property,UUID)]): PartSet = kvs match
    case Seq() => this
    case (f,id) +: tail => remProp(f,id).remProps(tail)

  /** Move the id `i` to the front of the list of ids.
    *
    * This is used, for instance, when dragging a sprite so that the sprite goes
    * over the other parts.
    */
  def moveFront(i: UUID) = {
    this.copy(
      ids = ids.filterNot(_ == i) :+ i
    )
  }

  def setData(kvs:Seq[(UUID,PropMap)]) = this.copy(
    propStore = propStore ++ kvs
  )

  /** Move the id `i` to the index `j` in the list of ids.
    *
    * This is used, for instance, when setting the position of a port.
    */
  def moveToIndex(i: UUID, j: Int) = {
    println(s"PartSet moveToIndex $i:${ids.indexOf(i)} -> $j")
    println(s"before: $ids")
    val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
    println(s"seg1 = $seg1, seg2 = $seg2")
    val ret = this.copy(
      ids = (seg1 :+ i) ++ seg2
    )
    println(s"after partset: ${ret.ids}")
    ret
  }

  def contains(id:UUID) = ids.contains(id)


}
object PartSet:
  /* Validataion */
  def apply(ids:Seq[UUID],propStore:PropStore): PartSet =
    assert(ids.toSet == propStore.keySet)
    new PartSet(ids,propStore.withDefaultValue(PropMap()))
  
  /* Convenience methods */
  def apply(propStore:PropStore): PartSet =
    apply(propStore.keys.toSeq,propStore)
  def apply(ids:Seq[UUID]): PartSet =
    apply(ids,ids.map((_,PropMap())).toMap)
  def apply(): PartSet = PartSet(Seq(),Map())




