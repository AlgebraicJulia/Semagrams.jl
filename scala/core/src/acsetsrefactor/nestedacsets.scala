// package semagrams.nestedacsets

// import semagrams._
// import semagrams.acsets._
// import cats.data.State
// import scala.collection.mutable
// import upickle.default._
// import monocle.Lens
// import semagrams.util.msgError
// import semagrams.util.bijectiveRW
// import semagrams.util.Complex

// /** A trait marking objects in a [[NestedSchema]] */
// trait NestedOb extends AbstractOb[NestedSchema]{

//   /** The subschema for the subacsets on parts of this type.
//     *
//     * The value is lazy in case an object is included in its own schema.
//     */
//   lazy val schema: NestedSchema = SchEmpty

//   /** This object promoted to type for the domain/codomain of a morphism */
//   def asDom() = Seq(partType)

//   def partType: PartType = PartType(Seq(this))

//   def extend(x: NestedOb) = partType.extend(x)

// }

// /** A trait marking morphisms in a [[NestedSchema]]
//   *
//   * Unlike in typical categories, morphisms can have multiple domains and
//   * multiple codomains. This is shorthand for a morphism for each pair of domain
//   * and codomain.
//   *
//   * Additionally, domains/codomains for morphisms are sequences of objects,
//   * representing paths through the schema, so a morphism goes between multiple
//   * levels of the schema.
//   */
// trait NestedHom extends AHom[NestedSchema] with Property {

//   /** The possible domains of this morphism */
//   val doms: Seq[PartType]

//   /** The possible codomains of this morphism */
//   val codoms: Seq[PartType]

//   type Value = Part

//   /** Any instance of [[Property]] needs a serializer/deserializer.
//     *
//     * This is a temporary hack that works in a specific case.
//     *
//     * @todo
//     *   figure out nested acset serialization
//     */
//   val rw = summon[ReadWriter[Int]].bimap(
//     _.path(0)._2.id + 1,
//     i => Part(Seq((codoms(0).path(0), Id(i - 1))))
//   )

//   /** Check whether `this` has `p` in the domain and `q` in the codomain,
//     * possibly nested within other parts.
//     */
//   def canSet(p: Part, _q: Any) = _q match
//     case q: Part =>
//       (for
//         dom <- doms
//         codom <- codoms
//         if p.hasFinal(dom) & q.hasFinal(codom)
//         pOverlap = p.path.dropRight(dom.path.length)
//         qOverlap = q.path.dropRight(codom.path.length)
//         if pOverlap == qOverlap
//       yield ()).nonEmpty

//     case _ => false

// }

// /** A trait marking attributes in a [[NestedSchema]]
//   *
//   * Unlike in Julia ACSets, we do not have AttrTypes. Rather, attributes have
//   * their associated scala type given by Property.Value.
//   */
// trait NestedAttr extends Property {

//   /** Like [[NestedHom]], this can have multiple domains; we interpret this
//     * mathematically as a separate attribute for each domain, but this makes it
//     * easier to write down.
//     */
//   val doms: Seq[PartType]

//   /** Check whether `this` has `p` in the domain, possibly nested within other
//     * parts.
//     */
//   def canSet(p: Part, v: Any): Boolean =
//     doms.exists(dom => p.hasFinal(dom)
//     // TODO: how to check this at runtime?
//     //  & a.isInstanceOf[Value]
//     )
// }

// /** The type of a part in a nested acset is a sequence of objects, going down
//   * through the nested schemas.
//   *
//   * So `PartType(Seq())` is the type of the root acset part, and
//   * `PartType(Seq(Box,InPort))` refers to the type of input ports on boxes.
//   */
// case class PartType(path: Seq[NestedOb]) extends EntityType {

//   /** Add an object to the end of the PartType */
//   def extend(x: NestedOb): PartType = PartType(path :+ x)

//   /** The first object in the PartType */
//   def head: NestedOb = path(0)

//   /** The PartType relative to the first object */
//   def tail: PartType = PartType(path.tail)

//   /** The last object in the PartType */
//   def last: NestedOb = path.last

//   /** The initial segment of the PartType */
//   def init: PartType = PartType(path.init)

//   /** Returns if `this` an extension of `that`? */
//   def >(that: PartType): Boolean = that.path match
//     case Seq() => true
//     case Seq(thathead, thattail @ _*) =>
//       head == thathead && tail > PartType(thattail)

//   /** Returns if `that` an extension of `this`? */
//   def <(that: PartType): Boolean = that > this

//   /** Return `Some(p)` if `this == that.extend(p)`, else `None` */
//   def diffOption(that: PartType): Option[PartType] = that.path match
//     case Seq()                       => Some(this)
//     case _ if this.head == that.head => this.tail.diffOption(that.tail)
//     case _                           => None

//   /** Return `p` if `this == that.extend(p)`, else error */
//   def -(that: PartType) = diffOption(that).getOrElse(
//     throw msgError(s"PartType $this is not an extension of $that")
//   )

// }

// object PartType {
//   given obIsPartType: Conversion[NestedOb, PartType] = (ob: NestedOb) => PartType(Seq(ob))
//   given seqIsPartType: Conversion[Seq[NestedOb], PartType] = PartType(_)

// }



// /** A part is a path through a nested acset. If you visualize a nested acset as
//   * a tree, where each node is an acset and its children are all of its
//   * subacsets, then a part tells at each level which subacset to choose.
//   *
//   * The empty path refers to the root acset.
//   *
//   * @todo
//   *   We should have "relative" parts and "absolute" parts, just like in a
//   *   filesystem there are relative and absolute paths. I think that currently
//   *   `Part` is used for both notions, which is very confusing.
//   */
// case class Part(path: Seq[(NestedOb, Id)]) extends Entity with Part {

//   /** All of the objects of the path */
//   override val ty: PartType = PartType(path.map(_._1))

//   /** Provide directions to go one acset deeper */
//   def extend(x: NestedOb, i: Id) = Part(path :+ (x, i))

//   /** Overload to return a Part rather than Entity */
//   def extendPart(p: Part): Part = Part(path ++ p.path)

//   /** Returns the first part in the path */
//   def head: Part = Part(path.slice(0, 1))

//   /** Returns the first object in the path */
//   def headOb: NestedOb = ty.path.head

//   /** Returns the first id in the path */
//   def headId: Id = path.head._2

//   /** Returns the tail segment of a part */
//   def tail: Part = Part(path.tail)

//   /** Returns the last part of the path */
//   def last: Part = Part(Seq(path.last))

//   /** Returns the last object in the path */
//   def lastOb: NestedOb = ty.path.last

//   /** Returns the last id in the path */
//   def lastId: Id = path.last._2

//   /** Returns the initial segment of the path */
//   def init: Part = Part(path.init)

//   /** Checks if `this` is more specific than `that` */
//   def >(that: Part): Boolean = that.path match
//     case Seq()                       => true
//     case _ if this.head == that.head => this.tail > that.tail
//     case _                           => false

//   /** Checks if `that` is more specific than `this` */
//   def <(that: Part): Boolean = that > this

//   /** Returns `Some(p)` if `this == that.extendPart(p)`, else `None` */
//   def diffOption(that: Part): Option[Part] =
//     that.path match
//       case Seq()                       => Some(this)
//       case _ if this.head == that.head => this.tail.diffOption(that.tail)
//       case _                           => None

//   /** Returns `p` if `this == that.extendPart(p)`, else errors */
//   def -(that: Part): Part = diffOption(that).getOrElse(
//     throw msgError(s"Part $this is not an extension of $that")
//   )

//   /** Checks if `ptype` is an initial segment of `ty` */
//   def hasInitial(ptype: PartType): Boolean = (this.path, ptype.path) match
//     case (_, Seq()) => true
//     case (Seq(), _) => false
//     case (_, phead +: ptail) =>
//       phead == headOb & tail.hasInitial(PartType(ptail))

//   /** Checks if `ptype` is an final segment of `ty` */
//   def hasFinal(ptype: PartType): Boolean = (this.path, ptype.path) match
//     case (_, Seq()) => true
//     case (Seq(), _) => false
//     case (_, pinit :+ plast) =>
//       plast == lastOb & init.hasFinal(PartType(pinit))

//   /** Transform to an name that is usable in tikz */
//   def tikzName: String =
//     path match
//       case Seq() =>
//         throw msgError(s"Can't use `ROOT` as a tikz location identifier")
//       case Seq((ob, id)) => ob.toString() + id.id.toString()
//       case _             => last.tikzName + "@" + init.tikzName

// }

// /** The schema for a nested acset.
//   *
//   * The nested part comes in because anything that implements `NestedOb` has another
//   * schema attached to it.
//   *
//   * @todo
//   *   What does this correspond to categorically?
//   */
// trait NestedSchema {
//   val obs: Seq[NestedOb]
//   val homs: Seq[NestedHom]
//   val attrs: Seq[NestedAttr]
//   val props: Seq[Property] = Seq()

//   /** Returns the subschema found by following the path in `ty`. */
//   def subschema(ty: PartType): NestedSchema = ty.path match {
//     case Nil => this
//     case ob :: rest => {
//       assert(obs contains ob)
//       ob.schema.subschema(PartType(rest))
//     }
//   }

//   /** Returns all subschemas of the `schemas` */
//   def subschemas(schemas: Set[NestedSchema] = Set()): Set[NestedSchema] =
//     if schemas.contains(this)
//     then schemas
//     else
//       val next = schemas ++ Set(this)
//       next ++ obs.flatMap(_.schema.subschemas(next))

//   /** Returns all of the homs that go into the given part type Each hom is
//     * prefixed by a path of objects needed to get to that hom
//     */
//   def homsInto(ty: PartType): Seq[(Seq[NestedOb], NestedHom)] = ty.path match {
//     case Nil => Seq()
//     case ob :: rest =>
//       homs.filter(_.codoms contains ty).map((Seq(), _))
//         ++ ob.schema
//           .homsInto(PartType(rest))
//           .map({ case (obs, f) => (ob +: obs, f) })
//   }

//   /** Create an empty NestedACSet from this schema. */
//   def apply(): NestedACSet = NestedACSet(this)

//   /** Serialization via upickle. This must live in the schema in order to know
//     * which typesafe objects/homs/attrs are available.
//     *
//     * bijectiveRW(seq) serializes to String using the built-in toString
//     *
//     * macroRW is the built-in upickle creation for case classes, but requires
//     * access to a background ReadWriter[NestedOb]
//     */

//   /** Serialization of `NestedOb`s in the schema */
//   implicit def obRW: ReadWriter[NestedOb] = bijectiveRW(obs)

//   /** Serialization of `PartType`s in the schema */
//   implicit def partTypeRW: ReadWriter[PartType] = macroRW

//   /** Serialization of `Part`s in the schema */
//   implicit def partRW: ReadWriter[Part] = macroRW

//   /** A superset of properties (`Property`) occuring in the schema. For
//     * serialization.
//     */
//   def allProps = (homs ++ attrs
//     ++ props ++ obs.flatMap(_.schema.props)
//     ++ GenericProperty.values).toSet

//   /** Serialization of properties (`Property`) in the schema */
//   implicit def propertyRW: ReadWriter[Property] = bijectiveRW(allProps)

//   /** Serialization of `PropMap`s in the schema, writing json objects for
//     * `Part`s and values for other properties.
//     */
//   implicit def propRW: ReadWriter[PropMap] =
//     readwriter[Map[String, ujson.Value]].bimap(
//       pm =>
//         pm.pmap.map {
//           case (k, v: Part) =>
//             (k.toString, writeJs(v))
//           case (k, v) =>
//             (k.toString, k.writeValue(v))
//         },
//       mp =>
//         PropMap(mp.map { case (k, v) =>
//           val prop = allProps
//             .find(write(_) == write(k))
//             .getOrElse(throw msgError(s"bad propRW $mp"))
//           prop match
//             case _: NestedHom =>
//               prop -> read[Part](v)
//             case _ =>
//               prop -> prop.readValue(v)
//         })
//     )

//   /** Serialization of `NestedSchema`s occuring in the schema */
//   implicit def schemaRW: ReadWriter[NestedSchema] = bijectiveRW(this.subschemas())

//   /** Serialization of `PartSet`s occuring in the schema. Note that this omits
//     * the `nextID` and rebuilds it from the sequence of ids.
//     */
//   implicit def partsRW: ReadWriter[PartSet] =
//     readwriter[(Seq[Int], Map[Int, ujson.Value])].bimap(
//       ps =>
//         (
//           ps.ids.map(_.id),
//           ps.ids.map(id => id.id -> writeJs(ps.acsets(id))).toMap
//         ),
//       (seq, dict) =>
//         PartSet(
//           seq.maxOption.map(_ + 1).getOrElse(0),
//           seq.map(Id(_)),
//           dict.map((i, json) => Id(i) -> read[NestedACSet](json))
//         )
//     )

//   /** Note: replacing `read[ujson.Value](write(acset.partsMap))` in `acsetRW`
//     * with `writeJs(acset.partsMap)` introduces a parsing error `expecting
//     * string but found int32`
//     */

//   /** Serialization of `ACSets`s based on the schema */
//   implicit def acsetRW: ReadWriter[NestedACSet] =
//     readwriter[Map[String, ujson.Value]].bimap(
//       acset =>
//         Map(
//           "schema" -> writeJs(acset.schema),
//           "props" -> writeJs(acset.props),
//           "partsMap" -> read[ujson.Value](write(acset.partsMap))
//         ),
//       jmap =>
//         NestedACSet(
//           read[NestedSchema](jmap("schema")),
//           read[PropMap](jmap("props")),
//           read[Map[NestedOb, PartSet]](jmap("partsMap"))
//         )
//     )

//   /** Serializer that includes runtime information (e.g., window size) */
//   def runtimeSerializer[A: ReadWriter](
//       key: String,
//       a: A
//   ): ReadWriter[(NestedACSet, A)] =
//     readwriter[Map[String, ujson.Value]].bimap(
//       (acset, a) =>
//         Map(
//           "acset" -> writeJs(acset),
//           key -> writeJs[A](a)
//         ),
//       jmap =>
//         (
//           read[NestedACSet](jmap("acset")),
//           read[A](jmap(key))
//         )
//     )

// }

// /** Storage class for the parts corresponding to an `NestedOb` in a schema.
//   *
//   * This is immutable; methods that logically mutate simply return new objects.
//   *
//   * @param nextId
//   *   The id to assign to the next part added
//   *
//   * @param ids
//   *   The ids of all the parts added so far. This is a `Seq` because we care
//   *   about the ordering; when the NestedACSet is displayed this ordering is used when
//   *   two sprites overlap.
//   *
//   * @param acsets
//   *   The subacset corresponding to each id
//   */
// case class PartSet(
//     nextId: Int,
//     ids: Seq[Id],
//     acsets: Map[Id, NestedACSet]
// ) {

//   /** Add multiple new parts, with subacsets given in `partacsets`.
//     *
//     * Returns a sequence of the ids of the added parts.
//     */
//   def addParts(partacsets: Seq[NestedACSet]): (PartSet, Seq[Id]) = {
//     val newIds = nextId.to(nextId + partacsets.length - 1).map(Id.apply)
//     val newPd = PartSet(
//       nextId + partacsets.length,
//       ids ++ newIds,
//       acsets ++ newIds.zip(partacsets).toMap
//     )
//     (newPd, newIds)
//   }

//   /** Adds a single part with subacset `acs`, returns its id. */
//   def addPart(acset: NestedACSet): (PartSet, Id) = {
//     val (p, newIds) = addParts(Seq(acset))
//     (p, newIds(0))
//   }

//   /** Set the subacset corresponding to `i` to `acs` */
//   def setAcset(i: Id, acs: NestedACSet): PartSet = {
//     this.copy(
//       acsets = acsets + (i -> acs)
//     )
//   }

//   /** Remove the part with id `i` */
//   def remPart(i: Id) = {
//     this.copy(
//       ids = ids.filterNot(_ == i),
//       acsets = acsets.filterNot(_._1 == i)
//     )
//   }

//   /** Move the id `i` to the front of the list of ids.
//     *
//     * This is used, for instance, when dragging a sprite so that the sprite goes
//     * over the other parts.
//     */
//   def moveFront(i: Id) = {
//     this.copy(
//       ids = ids.filterNot(_ == i) :+ i
//     )
//   }

//   /** Move the id `i` to the index `j` in the list of ids.
//     *
//     * This is used, for instance, when setting the position of a port.
//     */
//   def moveToIndex(i: Id, j: Int) = {
//     val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
//     this.copy(
//       ids = (seg1 :+ i) ++ seg2
//     )
//   }

// }

// /** The part corresponding to the top-level acset itself. */
// val ROOT = Part(Seq())

// /** The empty schema. */
// case object SchEmpty extends NestedSchema {
//   val obs = Seq()
//   val homs = Seq()
//   val attrs = Seq()
// }



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
//   *   the `PartSet` object for each `NestedOb` in the schema. This is where the
//   *   subacsets are stored.
//   */
// case class NestedACSet(
//     schema: NestedSchema,
//     props: PropMap,
//     partsMap: Map[NestedOb, PartSet]
// ) extends AbstractACSet[NestedSchema,NestedACSet] {
//   type Ob = nestedacsets.NestedOb
//   type Hom = nestedacsets.NestedHom | nestedacsets.NestedAttr
//   type Part = nestedacsets.Part

//   /** Get the subacset corresponding to a nested part; error if invalid */
//   def subacset(p: Part): NestedACSet = trySubacset(p).get

//   /** Get the subacset corresponding to a nested part; return None if invalid */
//   def trySubacset(p: Part): Option[NestedACSet] = p.path match {
//     case Nil => Some(this)
//     case (x, i) :: rest =>
//       val g = partsMap.get(x)
//       g.flatMap(parts =>
//         val aci = parts.acsets.get(i)
//         aci.flatMap(_.trySubacset(Part(rest)))
//       )
//   }

//   /** Check if a nested part exists in the NestedACSet */
//   def hasPart(p: Part): Boolean = p.path match {
//     case Nil => true
//     case (x, i) :: rest =>
//       (for {
//         parts <- partsMap.get(x)
//         sub <- parts.acsets.get(i)
//         res <- Some(sub.hasPart(Part(rest)))
//       } yield res).getOrElse(false)
//   }

//   /** Set the subacset for a nested part */
//   def setSubacset(p: Part, acs: NestedACSet): NestedACSet = p.path match {
//     case Nil => {
//       acs
//     }
//     case (x, i) :: rest => {
//       val parts = partsMap(x)
//       this.copy(
//         partsMap = partsMap + (x -> (parts
//           .setAcset(i, parts.acsets(i).setSubacset(Part(rest), acs))))
//       )
//     }
//   }

//   /** Return all of the parts of the subacset at `i` with type `x`, along with
//     * their corresponding subacsets.
//     */
//   def parts(i: Part, x: NestedOb): Seq[(Part, NestedACSet)] = {
//     val sub = subacset(i)
//     val ps = sub.partsMap
//       .get(x)
//       .getOrElse(
//         throw msgError(s"bad partsMap $x, ${sub.partsMap}")
//       )
//     ps.ids.map(id =>
//       (
//         i.extend(x, id),
//         ps.acsets
//           .get(id)
//           .getOrElse(
//             throw msgError(s"No acsets in $ps for $id")
//           )
//       )
//     )
//   }

//   /** Return all of the parts of the subacset at `i` with type `x`, without
//     * subacsets
//     */
//   def partsOnly(i: Part, x: NestedOb): Seq[Part] = {
//     val ps = subacset(i).partsMap(x)
//     ps.ids.map(id => i.extend(x, id))
//   }

//   /** Return parts and their properties, without subacsets **/
//   def partProps(i:Part,x:NestedOb): Seq[(Part,PropMap)] = parts(i,x)
//     .map((pt,acset) => (pt,acset.props))
//   def partProps(x:NestedOb): Seq[(Part,PropMap)] = partProps(ROOT,x)
//   /** Get the value of `f` at the part `i`; errors if unset. */
//   def subpart(f: Property, i: Part): f.Value = subacset(i).props(f)

//   /** Get the value of `f` at the part `i`; returns `None` if unset. */
//   def trySubpart(f: Property, i: Part): Option[f.Value] =
//     trySubacset(i).flatMap(_.props.get(f))

//   /** Check if the part `i` has property `f` */
//   def hasSubpart(f: Property, i: Part) = trySubpart(f, i) match
//     case Some(j) => true
//     case None    => false

//   /** Adds a part of type `x` to the subacset at `p` with initial subacset
//     * `init`
//     */
//   def addPart(p: Part, x: NestedOb, init: NestedACSet): (NestedACSet, Part) = {
//     val sub = subacset(p)
//     val (newparts, i) = sub.partsMap(x).addPart(init)
//     val newSub = sub.copy(
//       partsMap = sub.partsMap + (x -> newparts)
//     )
//     (setSubacset(p, newSub), p.extend(x, i))
//   }

//   /** Add several parts of type `x` to the subacset at `p` with initial
//     * subacsets given by inits.
//     */
//   def addParts(p: Part, x: NestedOb, inits: Seq[NestedACSet]): (NestedACSet, Seq[Part]) = {
//     val sub = subacset(p)
//     val (newparts, ids) = sub.partsMap(x).addParts(inits)
//     val newSub = sub.copy(
//       partsMap = sub.partsMap + (x -> newparts)
//     )
//     (setSubacset(p, newSub), ids.map(i => p.extend(x, i)))
//   }

//   /** Convenience overload of [[addParts]] */
//   def addPartsProps(p: Part, x: NestedOb, props: Seq[PropMap]): (NestedACSet, Seq[Part]) = {
//     val subschema = schema.subschema(p.ty.extend(x))
//     addParts(p, x, props.map(p => NestedACSet(subschema, p)))
//   }

//   /** Convenience overload of [[addPart]] */
//   def addPart(p: Part, x: NestedOb, props: PropMap): (NestedACSet, Part) = {
//     val subschema = schema.subschema(p.ty.extend(x))
//     addPart(p, x, NestedACSet(subschema, props))
//   }

//   /** Convenience overload of [[addPart]] */
//   def addPart(x: NestedOb, props: PropMap): (NestedACSet, Part) = addPart(ROOT, x, props)

//   /** Convenience overload of [[addPart]] */
//   def addPart(x: NestedOb, init: NestedACSet): (NestedACSet, Part) = addPart(ROOT, x, init)

//   /** Convenience overload of [[addPart]] */
//   def addPart(p: Part, x: NestedOb): (NestedACSet, Part) = addPart(p, x, PropMap())

//   /** Convenience overload of [[addPart]] */
//   def addPart(x: NestedOb): (NestedACSet, Part) = addPart(ROOT, x, PropMap())

//   /** Move the part `p` to the front of its parent `PartSet`. See
//     * [[PartSet.moveFront]].
//     */
//   def moveFront(p: Part): NestedACSet = {
//     assert(p.path.length > 0)
//     val (prefix, (x, i)) = (Part(p.path.dropRight(1)), p.path.last)
//     val sub = subacset(prefix)
//     val newsub = sub.copy(
//       partsMap = sub.partsMap + (x -> sub.partsMap(x).moveFront(i))
//     )
//     setSubacset(prefix, newsub)
//   }

//   /** Move the part `p` to the front of its parent `PartSet`. See
//     * [[PartSet.moveFront]].
//     */
//   def moveToIndex(p: Part, idx: Int): NestedACSet = {
//     val sub = subacset(p.init)
//     val newsub = sub.copy(
//       partsMap = sub.partsMap + (p.lastOb -> sub
//         .partsMap(p.lastOb)
//         .moveToIndex(p.lastId, idx))
//     )
//     setSubacset(p.init, newsub)
//   }

//   /** Set the property `f` of part `p` to `v` */
//   def setProp(p: Part, f: Property, v: f.Value): NestedACSet = {
//     val sub = subacset(p)
//     val newSub = sub.copy(
//       props = sub.props.set(f, v)
//     )
//     setSubacset(p, newSub)
//   }

//   /** Set the property `f` of part `p` to `v` */
//   def setSubpartProps(p: Part, pm: PropMap): NestedACSet = {
//     val sub = subacset(p)
//     val newSub = sub.copy(
//       props = sub.props ++ pm
//     )
//     setSubacset(p, newSub)
//   }

//   /** Set the property `f` of parts `ps` to `v` */
//   def setSubpartProps(ps: Seq[Part], pm: PropMap): NestedACSet = 
//     ps match
//       case Seq() => this
//       case head +: tail => setSubpartProps(head,pm).setSubpartProps(tail,pm)

//   /** Set the properties `pm` for all parts of type `ob` **/
//   def setGlobalProps(ob:NestedOb,pm:PropMap): NestedACSet =
//     setSubpartProps(partsOnly(ROOT,ob),pm)
    
//   /** Set properties `pm` on parts of type `ob` for (`ob`,`pm`) in `obProps` */
//   def setGlobalProps(obProps:Seq[(NestedOb,PropMap)]): NestedACSet =
//     obProps match
//       case Seq() => this
//       case (ob,props) +: tail => setGlobalProps(ob,props).setGlobalProps(tail)
    
//   /** Set the property `f` of parts `ps` to `v` if it is unset */
//   def softSetSubpart(p:Part,f:Property,v: f.Value) =
//     if hasSubpart(f,p) then this else setProp(p,f,v)

//   /** Set the property `f` of parts `ps` to `v` if it is unset */
//   def softSetSubpartProps(p:Part,pm:PropMap) =
//     setSubpartProps(p,pm.filterKeys(f => !hasSubpart(f,p)))

//   /** Set the properties `pm` on parts `ps` if they are unset */
//   def softSetSubpartProps(ps:Seq[Part],pm:PropMap): NestedACSet = ps match
//     case Seq() => this
//     case head +: tail => 
//       softSetSubpartProps(head,pm)
//         .softSetSubpartProps(tail,pm)
  
//   /** Set the properties `pm` on parts of type `ob` if they are unset */
//   def softSetGlobalProps(ob:NestedOb,pm:PropMap): NestedACSet = 
//     softSetSubpartProps(partsOnly(ROOT,ob),pm)

//   /** Set properties `pm` on parts of type `ob` for (`ob`,`pm`)
//    *  in `obProps` if they are unset */
//   def softSetGlobalProps(obProps:Seq[(NestedOb,PropMap)]): NestedACSet = obProps match
//     case Seq() => this
//     case (ob,props) +: tail => 
//       softSetGlobalProps(ob,props)
//         .softSetGlobalProps(tail)

//   /** Unset the property `f` of `p` */
//   def remSubpart(p: Part, f: Property): NestedACSet = {
//     val sub = subacset(p)
//     val newSub = sub.copy(
//       props = sub.props - f
//     )
//     setSubacset(p, newSub)
//   }

//   /** Return sequence of the parts that have property `f` set to `p` */
//   def incident(p: Part, f: NestedHom): Seq[Part] = {
//     val codom = 
      
//       f.codoms
//       .find(c => p.ty.path.drop(p.ty.path.length - c.path.length) == c.path)
//     val prefix = codom.map(x => Part(p.path.dropRight(x.path.length)))

//     /** Essentially, we look at all parts with part type f.dom, and filter which
//       * ones have a property f set to p
//       */
//     def helper(acs: NestedACSet, part: Part, remaining: Seq[NestedOb]): Seq[Part] =
//       remaining match {
//         case Nil => if acs.props.get(f) == Some(p) then Seq(part) else Seq()
//         case ob :: rest =>
//           acs
//             .partsMap(ob)
//             .acsets
//             .toSeq
//             .flatMap((i, acs) => helper(acs, part.extend(ob, i), rest))
//       }

//     prefix
//       .map(pfx => f.doms.flatMap(dom => helper(subacset(pfx), pfx, dom.path)))
//       .getOrElse(Seq())
//   }

//   /** Remove a part, but not any of the other parts that might refer to it. */
//   def remPartOnly(p: Part): NestedACSet = {
//     if hasPart(p) then {
//       assert(p.path.length > 0)
//       val (pre, (x, i)) = (p.path.dropRight(1), p.path.last)
//       val sub = subacset(Part(pre))
//       val newSub = sub.copy(
//         partsMap = sub.partsMap + (x -> sub.partsMap(x).remPart(i))
//       )
//       setSubacset(Part(pre), newSub)
//     } else {
//       this
//     }
//   }

//   /** Remove a part and all of the other parts that refer to it. */
//   def remPart(p: Part): NestedACSet = {
//     val visited = mutable.Set[Part]()
//     val queue = mutable.Queue[Part](p)
//     while (!queue.isEmpty) {
//       val q = queue.dequeue()
//       visited.add(q)
//       for ((_, f) <- schema.homsInto(q.ty)) {
//         queue.enqueueAll(
//           incident(q, f)
//             .filter(!visited.contains(_))
//         )
//       }
//       val sub = subacset(q)
//       for (ob <- sub.schema.obs) {
//         queue.enqueueAll(parts(q, ob).map(_._1).filter(!visited.contains(_)))
//       }
//     }
//     val toRemove = visited.toSeq
//     toRemove.foldLeft(this)(_.remPartOnly(_))
//   }

//   /** Remove all of the parts in `ps` */
//   def remParts(ps: Seq[Part]): NestedACSet = ps match
//     case Seq()             => this
//     case Seq(p, rest @ _*) => this.remPart(p).remParts(rest)

//   /** Add the properties in `newProps` to the top-level properties. */
//   def addProps(newProps: PropMap): NestedACSet = {
//     this.copy(props = props ++ newProps)
//   }

//   def allProps(p: Part): Set[Property] =
//     val sub = subacset(p)
//     val acsets = sub.partsMap.values
//       .map(_.acsets.values)
//       .flatten
//       .toSet

//     sub.props.pmap.keySet union
//       acsets.map(_.props.pmap.keySet).flatten

//   def scale(
//       from: Complex,
//       to: Complex,
//       scaleProps: Seq[Property { type Value = Complex }] = Seq(Center)
//   ): NestedACSet =
//     val oldProps = props.pmap.filter((k, _) => !scaleProps.contains(k))

//     val newProps = scaleProps
//       .filter(props.contains(_))
//       .map(k => (k, props(k).scaleFrom(from).scaleTo(to)))
//       .toMap

//     val newParts = partsMap.map((ob, pset) =>
//       (
//         ob,
//         pset.copy(
//           acsets =
//             pset.acsets.map((ob, acs) => (ob, acs.scale(from, to, scaleProps)))
//         )
//       )
//     )
//     this.copy(
//       props = PropMap(oldProps ++ newProps),
//       partsMap = newParts
//     )

// }

// /** This object contains the constructor method for ACSets and also a collection
//   * of wrappers around NestedACSet methods in the `State` monad that allow for a
//   * quasi-imperative API for modifying ACSets purely.
//   */
// object NestedACSet {

//   /** Construct a new NestedACSet with schema `s` */
//   def apply(s: NestedSchema): NestedACSet = NestedACSet(s, PropMap())

//   /** Construct a new NestedACSet with schema `s` and top-level parts `props` */
//   def apply(s: NestedSchema, props: PropMap): NestedACSet =
//     val pm = s.obs.map(ob => ob -> PartSet(0, Seq(), Map())).toMap
//     new NestedACSet(s, props, pm)

//   /** `State` wrapper around NestedACSet.addParts */
//   def addParts(p: Part, x: NestedOb, props: Seq[PropMap]): State[NestedACSet, Seq[Part]] =
//     State(_.addPartsProps(p, x, props))

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(p: Part, x: NestedOb, props: PropMap): State[NestedACSet, Part] =
//     State(_.addPart(p, x, props))

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(p: Part, x: NestedOb, init: NestedACSet): State[NestedACSet, Part] =
//     State(_.addPart(p, x, init))

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(p: Part, x: NestedOb): State[NestedACSet, Part] =
//     State(_.addPart(p, x))

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(x: NestedOb, props: PropMap): State[NestedACSet, Part] = State(
//     _.addPart(x, props)
//   )

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(x: NestedOb, init: NestedACSet): State[NestedACSet, Part] = State(
//     _.addPart(x, init)
//   )

//   /** `State` wrapper around NestedACSet.addPart */
//   def addPart(x: NestedOb): State[NestedACSet, Part] = State(_.addPart(x, PropMap()))

//   /** `State` wrapper around NestedACSet.setProp */
//   def setProp(p: Part, f: Property, v: f.Value): State[NestedACSet, Unit] =
//     State.modify(_.setProp(p, f, v))

//   /** `State` wrapper around NestedACSet.remSubpart */
//   def remSubpart(p: Part, f: Property): State[NestedACSet, Unit] =
//     State.modify(_.remSubpart(p, f))

//   /** `State` wrapper around NestedACSet.remPart */
//   def remPart(p: Part): State[NestedACSet, Unit] = State.modify(_.remPart(p))

//   /** `State` wrapper around NestedACSet.remParts */
//   def remParts(ps: Seq[Part]): State[NestedACSet, Unit] = State.modify(_.remParts(ps))

//   /** `State` wrapper around NestedACSet.moveFront */
//   def moveFront(p: Part): State[NestedACSet, Unit] = State.modify(_.moveFront(p))

//   /** Returns a lens into the value of the property `f` for part `x` */
//   def subpartLens(f: Property, x: Part) =
//     Lens[NestedACSet, f.Value](_.subpart(f, x))(y => s => s.setProp(x, f, y))
// }


