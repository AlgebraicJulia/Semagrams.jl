package semagrams.acsets

import semagrams._
import semagrams.util._
import semagrams.partprops._

import upickle.default._
import scala.annotation.targetName

type DataStore[D] = Map[UID, D]
extension [D: PartData](dstore: DataStore[D])
  def remProp(id: UID, f: Property) =
    dstore + (id -> (dstore(id).remProp(f)))
  def setProp(id: UID, f: Property, v: f.Value) =
    dstore + (id -> (dstore(id).setProp(f, v)))

/** Storage class for the parts corresponding to an `Ob` in a schema.
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
case class PartSet[D: PartData](
    ids: Seq[UID],
    dataStore: DataStore[D]
) {

  /* Getting */

  /* Check if `parts` are contained in the `PartSet` */
  def hasParts(parts: Iterable[UID]) = parts.forall(ids.contains)
  def hasPart(id: UID) = ids.contains(id)

  def contains(id: UID) = ids.contains(id)

  def contains(id: UID, f: Property): Boolean = contains(id) &
    dataStore(id).contains(f)

  /* Get the data associated with a part (or empty) */
  def getData(i: UID): D =
    dataStore.get(i).getOrElse(PartData())

  /* Get all properties for part `i` */
  def getProps(i: UID): PropMap = getData(i).getProps()

  /* Return an optional value for `f` on part `i` */
  def tryProp(f: Property, i: UID) =
    getProps(i).get(f)

  def hasProp(f: Property, i: UID) =
    tryProp(f, i).isDefined

  /* Get the value for `f` for part `i` (unsafe) */
  def getProp(f: Property, i: UID) =
    getProps(i)(f)

  /* Setting */

  /* Individual properties */

  /* Set `f` for `i` to `v` */
  def setProp(f: Property, id: UID, v: f.Value): PartSet[D] =
    setProp(f, Seq(id -> v))

  def setProp(f: Property, kvs: Iterable[(UID, f.Value)]): PartSet[D] =
    this.copy(
      dataStore =
        dataStore ++ kvs.map((id, v) => (id -> dataStore(id).setProp(f, v)))
    )

  def setProps(id: UID, props: PropMap): PartSet[D] =
    setProps(Seq(id -> props))

  def setProps(kvs: Iterable[(UID, PropMap)]): PartSet[D] = this.copy(
    dataStore =
      dataStore ++ kvs.map((id, props) => id -> dataStore(id).setProps(props))
  )

  def softSetProp(f: Property, id: UID, v: f.Value): PartSet[D] =
    if hasProp(f, id) then this else setProp(f, id, v)

  def softSetProp(f: Property, kvs: Iterable[(UID, f.Value)]): PartSet[D] =
    this.copy(
      dataStore =
        dataStore ++ kvs.map((id, v) => (id -> dataStore(id).softSetProp(f, v)))
    )

  def softSetProps(id: UID, props: PropMap): PartSet[D] =
    softSetProps(Seq(id -> props))

  def softSetProps(kvs: Iterable[(UID, PropMap)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((id, props) =>
      id -> dataStore(id).softSetProps(props)
    )
  )

  /* Set `f` for `i` to `v` */
  def remProp(f: Property, i: UID): PartSet[D] = this.copy(
    dataStore = dataStore - i
  )

  /* _ + `f:i -> v` sets `f` to `v` for `i` */
  def +[T](assignment: PartVal[UID, T]) =
    val PartVal(f, id, v) = assignment
    setProp(f, id, v)

  /* Remove the property `f` from `i` */
  def -(f: Property, i: UID) =
    remProp(f, i)

  /* Optionally set `f` to `v` on `i`, conditional on `pred` */
  def optionalSet(
      f: Property,
      i: UID,
      pred: Option[f.Value] => Boolean,
      v: f.Value
  ) = this.copy(
    dataStore = dataStore + (i -> dataStore(i).optionalSet(f, pred, v))
  )

  /* Conditionally set `f` to `v` on `i`, conditional on pred or if the value is missing */
  def conditionalSet(
      f: Property,
      i: UID,
      pred: f.Value => Boolean,
      v: f.Value
  ) = this.copy(
    dataStore = dataStore + (i -> dataStore(i).conditionalSet(f, pred, v))
  )

  /* Merging data */

  /* Merge the data in `kvs` into `dataStore` */
  def merge[Data2: PartData](kvs: Seq[(UID, Data2)]) = this.copy(
    dataStore = dataStore ++ kvs.map((id, d2) => id -> dataStore(id).merge(d2))
  )

  def ++[Data2: PartData](kvs: Seq[(UID, Data2)]) = merge(kvs)
  def ++[Data2: PartData](d2: Data2) = merge(ids.map(_ -> d2))

  def ++(f: Property, v: f.Value) = merge(ids.map(_ -> PropMap().set(f, v)))

  /** Merge unset data for  `i` with `d2` */
  def softMerge[Data2: PartData](kvs: Seq[(UID, Data2)]): PartSet[D] =
    this.copy(
      dataStore =
        dataStore ++ kvs.map((i, d2) => i -> dataStore(i).softMerge(d2))
    )

  /** Merge unset data for  `i` with `d2` */
  def softMerge[Data2: PartData](d2: Data2): PartSet[D] =
    softMerge(ids.map(_ -> d2))(summon[PartData[Data2]])

  /** Overwrite the data for a sequence of `id -> data` pairs */
  def resetData(kvs: Seq[(UID, D)]): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs
  )

  /** Merge unset data for  `i` with `d2` */
  def resetData(id: UID, d0: D): PartSet[D] =
    resetData(Seq(id -> d0))

  def remProps(kvs: Seq[(UID, Seq[Property])]) = this.copy(
    dataStore =
      dataStore ++ kvs.map((id, fs) => id -> dataStore(id).remProps(fs))
  )

  def --(kvs: Seq[(UID, Seq[Property])]) = remProps(kvs)

  @targetName("remPropsAllIds")
  def --(props: Seq[Property]) = remProps(ids.map(_ -> props))

  /* Optionally set `f` for `i -> v` pairs, conditional on `pred` for the current value */
  def optionalSet(
      f: Property,
      pred: Option[f.Value] => Boolean,
      kvs: Seq[(UID, f.Value)]
  ) = this.copy(
    dataStore =
      dataStore ++ kvs.map((i, v) => i -> dataStore(i).optionalSet(f, pred, v))
  )
  def optionalSet(
      f: Property,
      pred: Option[f.Value] => Boolean,
      v: f.Value
  ): PartSet[D] =
    optionalSet(f, pred, ids.map(_ -> v))

  /* Conditionally set `f` for `i -> v` pairs, conditional on `pred` or if the value is missing */
  def conditionalSet(
      f: Property,
      pred: f.Value => Boolean,
      kvs: Seq[(UID, f.Value)]
  ): PartSet[D] = this.copy(
    dataStore = dataStore ++ kvs.map((i, v) =>
      i -> dataStore(i).conditionalSet(f, pred, v)
    )
  )
  def conditionalSet(
      f: Property,
      pred: f.Value => Boolean,
      v: f.Value
  ): PartSet[D] =
    conditionalSet(f, pred, ids.map(_ -> v))

  /* Adding, moving & removing parts */

  /* Add a sequence of new parts with associated data */
  def addParts(parts: Iterable[(UID, D)]): PartSet[D] = PartSet(
    parts.map(_._1).toSeq ++ ids,
    parts.toMap ++ dataStore
  )

  /** Adds a single part with a given `id` and `data` */
  def addPart(id: UID, data: D): PartSet[D] =
    addParts(Seq(id -> data))

  /* Remove a sequence of parts with the ids `rem` */
  def remParts(rem: Iterable[UID]): PartSet[D] = PartSet(
    ids.diff(rem.toSeq),
    dataStore -- rem
  )

  /** Remove a single part with a given `id` */
  def remPart(id: UID): PartSet[D] = remParts(Seq(id))

  /* Move id `i` to the index `j` in the list of ids. Used to
   * change rendering order and ordering of ports
   */
  def moveToIndex(i: UID, j: Int) = {
    val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
    this.copy(
      ids = (seg1 :+ i) ++ seg2
    )
  }

  /* Move id `i` to the front of `ids`. */
  def moveToEnd(i: UID) = this.copy(
    ids = ids.filterNot(_ == i) :+ i
  )

  /* Move id `i` to the end of `ids`. */
  def moveToFront(i: UID) = this.copy(
    ids = i +: ids.filterNot(_ == i)
  )

}
object PartSet:
  /* Validataion */
  def apply[D: PartData](ids: Seq[UID], dataStore: DataStore[D]): PartSet[D] =
    assert(ids.toSet == dataStore.keySet)
    new PartSet(ids, dataStore.withDefaultValue(PartData()))

  /* Convenience methods */
  def apply[D: PartData](dataStore: DataStore[D]): PartSet[D] =
    apply(dataStore.keys.toSeq, dataStore)
  def apply[D: PartData](ids: Seq[UID]): PartSet[D] =
    apply(ids, ids.map((_, PartData())).toMap)
  def apply[D: PartData](): PartSet[D] = PartSet(Seq(), Map())

type PartStore[D] = Map[UID, PartSet[D]]

object PartStore:
  def apply[D: PartData]() = Map[UID, PartSet[D]]().withDefaultValue(PartSet())

  implicit val rw: ReadWriter[Part] =
    readwriter[(UID, UID, String)].bimap[Part](
      part => (part.id, part.ob.id, part.ob.label),
      (partid, obid, obname) => Part(partid, Table(obid, obname))
    )
