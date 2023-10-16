// package semagrams.dependentacsets

// import semagrams._
// import semagrams.acsets.{Ob => Ob0,Hom => Hom0,Attr => Attr0}
// import cats.data.State
// import scala.collection.mutable
// import upickle.default._
// import monocle.Lens
// import semagrams.util.msgError
// import semagrams.util.bijectiveRW
// import semagrams.util.Complex

// /* Atomic or complex schema elements */
// sealed trait SchemaElt:
//   def generators: Seq[Generator]
//   def label: String

// type Pt[X] = X & Singleton

// /** Categorical typing **/
// /* Schema objects */
// // sealed trait SchemaOb extends Ob0 with SchemaElt:
// //   def id: SchemaArrow[this.type]
// // sealed trait TableOb extends SchemaOb:
// //   def id = TableId(this)
// // sealed trait TypeOb extends SchemaOb:
// //   def id = TypeId(this)
// //   def factors: Seq[SimpleType[_]]


// /* Schema arrows */
// // sealed trait SchemaArrow[x<:Pt[SchemaOb]] 
// //   extends SchemaElt:
// //   val dom: x
// //   val codom: SchemaOb
//   // def compose(f:SchemaArrow[codom.type]): SchemaArrow[dom.type]
//     // SeqArrow(dom,this.path ++ f.path)
//     // SeqArrow[dom.type](dom,this.path ++ f.path)
//   // def *(f:SchemaArrow[codom.type]) = compose(f)

// /* TableOb -> TableOb */
// sealed trait Hom[o<:Pt[TableOb]] 
//   extends SchemaArrow[o]:
//   val codom: TableOb
//   // def path: Seq[Hom[_]]

// /* TableOb -> TypeOb */
// sealed trait Attr[o<:Pt[TableOb]]
//   extends SchemaArrow[o]:
//   val codom: TypeOb

// /* TypeOb -> TypeOb */
// sealed trait Comp[o<:Pt[TypeOb]]
//   extends SchemaArrow[o]:
//   val codom: TypeOb
//   def path: Seq[Comp[_]]

// /** Generators **/

// /* Atomic schema elements */
// sealed trait Generator:
//   def name: String
//   def label = name

// sealed trait ObjectGenerator extends Generator with SchemaOb:
//   def generators: Seq[Generator] = Seq(this)

// sealed trait ArrowGenerator[O<:SchemaOb,o<:Pt[O]] extends Generator with SchemaArrow[o]:
//   def generators = dom.generators ++ codom.generators :+ this
//   def path: Seq[_<:SchemaArrow[_]] = Seq(this)

// /* Object generators */
// case class SimpleTable(name:String)
//   extends TableOb with ObjectGenerator

// case class SimpleType[T:ReadWriter](name:String) 
//   extends TypeOb with ObjectGenerator:
//     def factors = Seq(this)

//   // def *(that:Attr) = ??? //ProdType(Seq(this,that))
//   // def *(that:ProdType) = ProdType(this +: that.attrs)

// /* Arrow generators */
// case class SimpleHom[o<:Pt[SimpleTable]](name:String,dom:o,codom:SimpleTable) 
//   extends Hom[o] with ArrowGenerator[TableOb,o]
//     // def compose(f:SchemaArrow[codom.type]) = f match
//     //   case f:Hom[codom.type] => HomSeq(dom,this +: f.path)

// case class SimpleAttr[o<:Pt[SimpleTable]](name:String,dom:o,codom:TypeOb) 
//   extends Attr[o] with ArrowGenerator[TableOb,o]

// // case class SimpleComp[tup<:Pt[TypeOb]](name:String,dom:tup,codom:TypeOb)
// //   extends ArrowGenerator[TypeOb,tup]




// /* Object Constructors */

// // case class ProdType(factors:Seq[SimpleType[_]]) 
// //   extends TypeOb:
// //     def generators = factors.flatMap(_.generators)
// //     def label = generators.map(_.label).mkString("×")

// // case class Query[tup<:Pt[TypeOb]](
// //   select:Seq[Attr],
// //   from:Seq[Attr],
// //   where:CompexComp[tup]
// // )

// // val UnitType = ProdType(Seq())

// /* Arrow constructors */

// /* Identities */
// sealed trait IdArrow[o<:Pt[SchemaOb]](val ob:o) extends SchemaArrow[o]:
//   // val dom = ob
//   // val codom = ob
//   def generators = ob.generators
//   // def label = s"id(${ob.label})"

//   // def compose(f:SchemaArrow[ob.type]) = f

// object IdArrow:
//   def IdArrow(ob:SchemaOb) = ob match
//     case ob:TableOb => TableId(ob)
//     case ob:TypeOb => TypeId(ob)

  
// // case class TableId[o<:Pt[TableOb]](ob:o) extends IdArrow[o](ob) with Hom[o]
// // case class TypeId[o<:Pt[TypeOb]](ob:o) extends IdArrow[o](ob) with Comp[o]

// /* Composition */
// sealed trait SeqArrow[o<:Pt[SchemaOb]](dom:o,path:Seq[SchemaArrow[_]])
//  extends SchemaArrow[o]:
//   assert(path.length > 1)
//   assert(path.map(_.dom) == dom +: path.map(_.codom).dropRight(1))
//   val codom = path.last.codom
//   def generators = path.flatMap(_.generators).distinct

// // object SeqArrow:
// //   def apply(fs:Seq[SchemaArrow[_]]) = 
// //     fs match
// //     case Seq() => throw msgError("Empty sequence arrow")
// //     case Seq(f) => f
// //     case head +: tail => head.compose(SeqArrow(tail))

// case class HomSeq[o<:Pt[TableOb]](dom:o,path:Seq[Hom[_]]) 
//   extends Hom[o]


// case class AttrSeq[o<:Pt[TableOb]](dom:o,hom:Hom[o],attr:Attr[_],comp:Comp[_])
//   extends Attr[o]
// // object AttrSeq:
// //   def apply(ob:TableOb,path:Seq[SchemaArrow[_]])



// case class CompSeq[o<:Pt[TypeOb]](dom:o,path:Seq[Comp[_]])
//   extends Comp[o]

// // case class SeqHom[o<:Pt[TableOb]]  


//   // val typeRW = readwriter[String].bimap[AttrType[T]](
//   //   _.name,
//   //   _ match
//   //     case name => this
//   //     case other =>
//   //       println(s"Warning: reading AttrType $other as $name")
//   //       this
//   // )

// /* Product types */
// // case class ProdType(attrs:Seq[AttrType[_]]) 
// //   extends TypeOb derives ReadWriter:
// //   def *(that:ProdType) = ProdType(attrs ++ that.attrs)
// //   def *(that:AttrType[_]) = ProdType(attrs :+ that)





// // case class Tuple[o<:(as:Seq[Attr])
// // case class SchemaId[O<:SchemaOb,o<:Pt[O]](ob:o) extends SchemaHom[o]

// /** A trait marking attributes in a [[Schema]]
//   *
//   * Unlike in Julia ACSets, we do not have AttrTypes. Rather, attributes have
//   * their associated scala type given by Property.Value.
// //   */
// // trait SimpleAttr[T:ReadWriter] extends Attr with PValue[T] {

// //   /** Like [[Hom]], this can have multiple domains; we interpret this
// //     * mathematically as a separate attribute for each domain, but this makes it
// //     * easier to write down.
// //     */
// //   val dom: TableOb
// //   val doms = dom.asDom()

// //   val codom = AttrType[T]
// // }



// // /** A part is a path through a nested acset. If you visualize a nested acset as
// //   * a tree, where each node is an acset and its children are all of its
// //   * subacsets, then a part tells at each level which subacset to choose.
// //   *
// //   * The empty path refers to the root acset.
// //   *
// //   * @todo
// //   *   We should have "relative" parts and "absolute" parts, just like in a
// //   *   filesystem there are relative and absolute paths. I think that currently
// //   *   `Part` is used for both notions, which is very confusing.
// //   */
// // case class SimplePart(ob:Ob,idTag:Id) extends Entity {

// //   def asPart = Part(Seq((ob,idTag)))
// //   def id = idTag.id

// //   /** All of the objects of the path */
// //   override val ty: PartType = asPart.ty


// //   /** Transform to an name that is usable in tikz */
// //   def tikzName: String = ob.toString() + id.toString()
// // }

// // object SimplePart:
// //   def apply(ob:Ob,i:Int) = new SimplePart(ob,Id(i))



// // case class SimpleSchema(
// //   obs: Seq[SchemaOb],
// //   homs: Seq[SimpleHom],
// //   attrs: Seq[SimpleAttr],
// //   override val props: Seq[Property]
// // ) extends Schema 



// /** An opaque wrapper around an integer */
// // case class Id(id: Int)
// // object Id {
// //   implicit val idRW: ReadWriter[Id] = readwriter[Int].bimap(
// //     _.id,
// //     Id(_)
// //   )
// // }

// // /** Storage class for the parts corresponding to an `Ob` in a schema.
// //   *
// //   * This is immutable; methods that logically mutate simply return new objects.
// //   *
// //   * @param nextId
// //   *   The id to assign to the next part added
// //   *
// //   * @param ids
// //   *   The ids of all the parts added so far. This is a `Seq` because we care
// //   *   about the ordering; when the ACSet is displayed this ordering is used when
// //   *   two sprites overlap.
// //   *
// //   * @param acsets
// //   *   The subacset corresponding to each id
// //   */
// // case class PartSet(
// //     nextId: Int,
// //     ids: Seq[Id],
// //     acsets: Map[Id, ACSet]
// // ) {

// //   /** Add multiple new parts, with subacsets given in `partacsets`.
// //     *
// //     * Returns a sequence of the ids of the added parts.
// //     */
// //   def addParts(partacsets: Seq[ACSet]): (PartSet, Seq[Id]) = {
// //     val newIds = nextId.to(nextId + partacsets.length - 1).map(Id.apply)
// //     val newPd = PartSet(
// //       nextId + partacsets.length,
// //       ids ++ newIds,
// //       acsets ++ newIds.zip(partacsets).toMap
// //     )
// //     (newPd, newIds)
// //   }

// //   /** Adds a single part with subacset `acs`, returns its id. */
// //   def addPart(acset: ACSet): (PartSet, Id) = {
// //     val (p, newIds) = addParts(Seq(acset))
// //     (p, newIds(0))
// //   }

// //   /** Set the subacset corresponding to `i` to `acs` */
// //   def setAcset(i: Id, acs: ACSet): PartSet = {
// //     this.copy(
// //       acsets = acsets + (i -> acs)
// //     )
// //   }

// //   /** Remove the part with id `i` */
// //   def remPart(i: Id) = {
// //     this.copy(
// //       ids = ids.filterNot(_ == i),
// //       acsets = acsets.filterNot(_._1 == i)
// //     )
// //   }

// //   /** Move the id `i` to the front of the list of ids.
// //     *
// //     * This is used, for instance, when dragging a sprite so that the sprite goes
// //     * over the other parts.
// //     */
// //   def moveFront(i: Id) = {
// //     this.copy(
// //       ids = ids.filterNot(_ == i) :+ i
// //     )
// //   }

// //   /** Move the id `i` to the index `j` in the list of ids.
// //     *
// //     * This is used, for instance, when setting the position of a port.
// //     */
// //   def moveToIndex(i: Id, j: Int) = {
// //     val (seg1, seg2) = ids.filterNot(_ == i).splitAt(j)
// //     this.copy(
// //       ids = (seg1 :+ i) ++ seg2
// //     )
// //   }

// // }

// // /** The part corresponding to the top-level acset itself. */
// // val ROOT = Part(Seq())

// // /** The empty schema. */
// // case object SchEmpty extends Schema {
// //   val obs = Seq()
// //   val homs = Seq()
// //   val attrs = Seq()
// // }

// // /** A nested acset.
// //   *
// //   * @param schema
// //   *   the schema that the acset conforms to
// //   *
// //   * @param props
// //   *   the top-level properties. The values of morphisms, attributes, and generic
// //   *   [[Property]]s are stored here. We don't need what in Catlab we call
// //   *   "subparts"; it's folded into this. For instance, if this is the subacset
// //   *   for an edge, then the source and target are stored here.
// //   *
// //   * @param partsMap
// //   *   the `PartSet` object for each `Ob` in the schema. This is where the
// //   *   subacsets are stored.
// //   */
// // case class ACSet(
// //     schema: Schema,
// //     props: PropMap,
// //     partsMap: Map[Ob, PartSet]
// // ) {

// //   /** Get the subacset corresponding to a nested part; error if invalid */
// //   def subacset(p: Part): ACSet = trySubacset(p).get

// //   /** Get the subacset corresponding to a nested part; return None if invalid */
// //   def trySubacset(p: Part): Option[ACSet] = p.path match {
// //     case Nil => Some(this)
// //     case (x, i) :: rest =>
// //       val g = partsMap.get(x)
// //       g.flatMap(parts =>
// //         val aci = parts.acsets.get(i)
// //         aci.flatMap(_.trySubacset(Part(rest)))
// //       )
// //   }

// //   /** Check if a nested part exists in the ACSet */
// //   def hasPart(p: Part): Boolean = p.path match {
// //     case Nil => true
// //     case (x, i) :: rest =>
// //       (for {
// //         parts <- partsMap.get(x)
// //         sub <- parts.acsets.get(i)
// //         res <- Some(sub.hasPart(Part(rest)))
// //       } yield res).getOrElse(false)
// //   }

// //   /** Set the subacset for a nested part */
// //   def setSubacset(p: Part, acs: ACSet): ACSet = p.path match {
// //     case Nil => {
// //       acs
// //     }
// //     case (x, i) :: rest => {
// //       val parts = partsMap(x)
// //       this.copy(
// //         partsMap = partsMap + (x -> (parts
// //           .setAcset(i, parts.acsets(i).setSubacset(Part(rest), acs))))
// //       )
// //     }
// //   }

// //   /** Return all of the parts of the subacset at `i` with type `x`, along with
// //     * their corresponding subacsets.
// //     */
// //   def parts(i: Part, x: Ob): Seq[(Part, ACSet)] = {
// //     val sub = subacset(i)
// //     val ps = sub.partsMap
// //       .get(x)
// //       .getOrElse(
// //         throw msgError(s"bad partsMap $x, ${sub.partsMap}")
// //       )
// //     ps.ids.map(id =>
// //       (
// //         i.extend(x, id),
// //         ps.acsets
// //           .get(id)
// //           .getOrElse(
// //             throw msgError(s"No acsets in $ps for $id")
// //           )
// //       )
// //     )
// //   }

// //   /** Return all of the parts of the subacset at `i` with type `x`, without
// //     * subacsets
// //     */
// //   def partsOnly(i: Part, x: Ob): Seq[Part] = {
// //     val ps = subacset(i).partsMap(x)
// //     ps.ids.map(id => i.extend(x, id))
// //   }

// //   /** Get the value of `f` at the part `i`; errors if unset. */
// //   def subpart(f: Property, i: Part): f.Value = subacset(i).props(f)

// //   /** Get the value of `f` at the part `i`; returns `None` if unset. */
// //   def trySubpart(f: Property, i: Part): Option[f.Value] =
// //     trySubacset(i).flatMap(_.props.get(f))

// //   /** Check if the part `i` has property `f` */
// //   def hasSubpart(f: Property, i: Part) = trySubpart(f, i) match
// //     case Some(j) => true
// //     case None    => false

// //   /** Adds a part of type `x` to the subacset at `p` with initial subacset
// //     * `init`
// //     */
// //   def addPart(p: Part, x: Ob, init: ACSet): (ACSet, Part) = {
// //     val sub = subacset(p)
// //     val (newparts, i) = sub.partsMap(x).addPart(init)
// //     val newSub = sub.copy(
// //       partsMap = sub.partsMap + (x -> newparts)
// //     )
// //     (setSubacset(p, newSub), p.extend(x, i))
// //   }

// //   /** Add several parts of type `x` to the subacset at `p` with initial
// //     * subacsets given by inits.
// //     */
// //   def addParts(p: Part, x: Ob, inits: Seq[ACSet]): (ACSet, Seq[Part]) = {
// //     val sub = subacset(p)
// //     val (newparts, ids) = sub.partsMap(x).addParts(inits)
// //     val newSub = sub.copy(
// //       partsMap = sub.partsMap + (x -> newparts)
// //     )
// //     (setSubacset(p, newSub), ids.map(i => p.extend(x, i)))
// //   }

// //   /** Convenience overload of [[addParts]] */
// //   def addPartsProps(p: Part, x: Ob, props: Seq[PropMap]): (ACSet, Seq[Part]) = {
// //     val subschema = schema.subschema(p.ty.extend(x))
// //     addParts(p, x, props.map(p => ACSet(subschema, p)))
// //   }

// //   /** Convenience overload of [[addPart]] */
// //   def addPart(p: Part, x: Ob, props: PropMap): (ACSet, Part) = {
// //     val subschema = schema.subschema(p.ty.extend(x))
// //     addPart(p, x, ACSet(subschema, props))
// //   }

// //   /** Convenience overload of [[addPart]] */
// //   def addPart(x: Ob, props: PropMap): (ACSet, Part) = addPart(ROOT, x, props)

// //   /** Convenience overload of [[addPart]] */
// //   def addPart(x: Ob, init: ACSet): (ACSet, Part) = addPart(ROOT, x, init)

// //   /** Convenience overload of [[addPart]] */
// //   def addPart(p: Part, x: Ob): (ACSet, Part) = addPart(p, x, PropMap())

// //   /** Convenience overload of [[addPart]] */
// //   def addPart(x: Ob): (ACSet, Part) = addPart(ROOT, x, PropMap())

// //   /** Move the part `p` to the front of its parent `PartSet`. See
// //     * [[PartSet.moveFront]].
// //     */
// //   def moveFront(p: Part): ACSet = {
// //     assert(p.path.length > 0)
// //     val (prefix, (x, i)) = (Part(p.path.dropRight(1)), p.path.last)
// //     val sub = subacset(prefix)
// //     val newsub = sub.copy(
// //       partsMap = sub.partsMap + (x -> sub.partsMap(x).moveFront(i))
// //     )
// //     setSubacset(prefix, newsub)
// //   }

// //   /** Move the part `p` to the front of its parent `PartSet`. See
// //     * [[PartSet.moveFront]].
// //     */
// //   def moveToIndex(p: Part, idx: Int): ACSet = {
// //     val sub = subacset(p.init)
// //     val newsub = sub.copy(
// //       partsMap = sub.partsMap + (p.lastOb -> sub
// //         .partsMap(p.lastOb)
// //         .moveToIndex(p.lastId, idx))
// //     )
// //     setSubacset(p.init, newsub)
// //   }

// //   /** Set the property `f` of part `p` to `v` */
// //   def setProp(p: Part, f: Property, v: f.Value): ACSet = {
// //     val sub = subacset(p)
// //     val newSub = sub.copy(
// //       props = sub.props.set(f, v)
// //     )
// //     setSubacset(p, newSub)
// //   }

// //   /** Set the property `f` of part `p` to `v` */
// //   def setSubpartProps(p: Part, pm: PropMap): ACSet = {
// //     val sub = subacset(p)
// //     val newSub = sub.copy(
// //       props = sub.props ++ pm
// //     )
// //     setSubacset(p, newSub)
// //   }

// //   /** Set the property `f` of parts `ps` to `v` */
// //   def setSubpartProps(ps: Seq[Part], pm: PropMap): ACSet = 
// //     ps match
// //       case Seq() => this
// //       case head +: tail => setSubpartProps(head,pm).setSubpartProps(tail,pm)

// //   /** Set the properties `pm` for all parts of type `ob` **/
// //   def setGlobalProps(ob:Ob,pm:PropMap): ACSet =
// //     setSubpartProps(partsOnly(ROOT,ob),pm)
    
// //   /** Set properties `pm` on parts of type `ob` for (`ob`,`pm`) in `obProps` */
// //   def setGlobalProps(obProps:Seq[(Ob,PropMap)]): ACSet =
// //     obProps match
// //       case Seq() => this
// //       case (ob,props) +: tail => setGlobalProps(ob,props).setGlobalProps(tail)
    
// //   /** Set the property `f` of parts `ps` to `v` if it is unset */
// //   def softSetSubpart(p:Part,f:Property,v: f.Value) =
// //     if hasSubpart(f,p) then this else setProp(p,f,v)

// //   /** Set the property `f` of parts `ps` to `v` if it is unset */
// //   def softSetSubpartProps(p:Part,pm:PropMap) =
// //     setSubpartProps(p,pm.filterKeys(f => !hasSubpart(f,p)))

// //   /** Set the properties `pm` on parts `ps` if they are unset */
// //   def softSetSubpartProps(ps:Seq[Part],pm:PropMap): ACSet = ps match
// //     case Seq() => this
// //     case head +: tail => 
// //       softSetSubpartProps(head,pm)
// //         .softSetSubpartProps(tail,pm)
  
// //   /** Set the properties `pm` on parts of type `ob` if they are unset */
// //   def softSetGlobalProps(ob:Ob,pm:PropMap): ACSet = 
// //     softSetSubpartProps(partsOnly(ROOT,ob),pm)

// //   /** Set properties `pm` on parts of type `ob` for (`ob`,`pm`)
// //    *  in `obProps` if they are unset */
// //   def softSetGlobalProps(obProps:Seq[(Ob,PropMap)]): ACSet = obProps match
// //     case Seq() => this
// //     case (ob,props) +: tail => 
// //       softSetGlobalProps(ob,props)
// //         .softSetGlobalProps(tail)

// //   /** Unset the property `f` of `p` */
// //   def remSubpart(p: Part, f: Property): ACSet = {
// //     val sub = subacset(p)
// //     val newSub = sub.copy(
// //       props = sub.props - f
// //     )
// //     setSubacset(p, newSub)
// //   }

// //   /** Return sequence of the parts that have property `f` set to `p` */
// //   def incident(p: Part, f: Hom): Seq[Part] = {
// //     val codom = f.codoms
// //       .find(c => p.ty.path.drop(p.ty.path.length - c.path.length) == c.path)
// //     val prefix = codom.map(x => Part(p.path.dropRight(x.path.length)))

// //     /** Essentially, we look at all parts with part type f.dom, and filter which
// //       * ones have a property f set to p
// //       */
// //     def helper(acs: ACSet, part: Part, remaining: Seq[Ob]): Seq[Part] =
// //       remaining match {
// //         case Nil => if acs.props.get(f) == Some(p) then Seq(part) else Seq()
// //         case ob :: rest =>
// //           acs
// //             .partsMap(ob)
// //             .acsets
// //             .toSeq
// //             .flatMap((i, acs) => helper(acs, part.extend(ob, i), rest))
// //       }

// //     prefix
// //       .map(pfx => f.doms.flatMap(dom => helper(subacset(pfx), pfx, dom.path)))
// //       .getOrElse(Seq())
// //   }

// //   /** Remove a part, but not any of the other parts that might refer to it. */
// //   def remPartOnly(p: Part): ACSet = {
// //     if hasPart(p) then {
// //       assert(p.path.length > 0)
// //       val (pre, (x, i)) = (p.path.dropRight(1), p.path.last)
// //       val sub = subacset(Part(pre))
// //       val newSub = sub.copy(
// //         partsMap = sub.partsMap + (x -> sub.partsMap(x).remPart(i))
// //       )
// //       setSubacset(Part(pre), newSub)
// //     } else {
// //       this
// //     }
// //   }

// //   /** Remove a part and all of the other parts that refer to it. */
// //   def remPart(p: Part): ACSet = {
// //     val visited = mutable.Set[Part]()
// //     val queue = mutable.Queue[Part](p)
// //     while (!queue.isEmpty) {
// //       val q = queue.dequeue()
// //       visited.add(q)
// //       for ((_, f) <- schema.homsInto(q.ty)) {
// //         queue.enqueueAll(
// //           incident(q, f)
// //             .filter(!visited.contains(_))
// //         )
// //       }
// //       val sub = subacset(q)
// //       for (ob <- sub.schema.obs) {
// //         queue.enqueueAll(parts(q, ob).map(_._1).filter(!visited.contains(_)))
// //       }
// //     }
// //     val toRemove = visited.toSeq
// //     toRemove.foldLeft(this)(_.remPartOnly(_))
// //   }

// //   /** Remove all of the parts in `ps` */
// //   def remParts(ps: Seq[Part]): ACSet = ps match
// //     case Seq()             => this
// //     case Seq(p, rest @ _*) => this.remPart(p).remParts(rest)

// //   /** Add the properties in `newProps` to the top-level properties. */
// //   def addProps(newProps: PropMap): ACSet = {
// //     this.copy(props = props ++ newProps)
// //   }

// //   def allProps(p: Part): Set[Property] =
// //     val sub = subacset(p)
// //     val acsets = sub.partsMap.values
// //       .map(_.acsets.values)
// //       .flatten
// //       .toSet

// //     sub.props.pmap.keySet union
// //       acsets.map(_.props.pmap.keySet).flatten

// //   def scale(
// //       from: Complex,
// //       to: Complex,
// //       scaleProps: Seq[Property { type Value = Complex }] = Seq(Center)
// //   ): ACSet =
// //     val oldProps = props.pmap.filter((k, _) => !scaleProps.contains(k))

// //     val newProps = scaleProps
// //       .filter(props.contains(_))
// //       .map(k => (k, props(k).scaleFrom(from).scaleTo(to)))
// //       .toMap

// //     val newParts = partsMap.map((ob, pset) =>
// //       (
// //         ob,
// //         pset.copy(
// //           acsets =
// //             pset.acsets.map((ob, acs) => (ob, acs.scale(from, to, scaleProps)))
// //         )
// //       )
// //     )
// //     this.copy(
// //       props = PropMap(oldProps ++ newProps),
// //       partsMap = newParts
// //     )

// // }

// // /** This object contains the constructor method for ACSets and also a collection
// //   * of wrappers around ACSet methods in the `State` monad that allow for a
// //   * quasi-imperative API for modifying ACSets purely.
// //   */
// // object ACSet {

// //   /** Construct a new ACSet with schema `s` */
// //   def apply(s: Schema): ACSet = ACSet(s, PropMap())

// //   /** Construct a new ACSet with schema `s` and top-level parts `props` */
// //   def apply(s: Schema, props: PropMap): ACSet =
// //     val pm = s.obs.map(ob => ob -> PartSet(0, Seq(), Map())).toMap
// //     new ACSet(s, props, pm)

// //   /** `State` wrapper around ACSet.addParts */
// //   def addParts(p: Part, x: Ob, props: Seq[PropMap]): State[ACSet, Seq[Part]] =
// //     State(_.addPartsProps(p, x, props))

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(p: Part, x: Ob, props: PropMap): State[ACSet, Part] =
// //     State(_.addPart(p, x, props))

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(p: Part, x: Ob, init: ACSet): State[ACSet, Part] =
// //     State(_.addPart(p, x, init))

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(p: Part, x: Ob): State[ACSet, Part] =
// //     State(_.addPart(p, x))

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(x: Ob, props: PropMap): State[ACSet, Part] = State(
// //     _.addPart(x, props)
// //   )

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(x: Ob, init: ACSet): State[ACSet, Part] = State(
// //     _.addPart(x, init)
// //   )

// //   /** `State` wrapper around ACSet.addPart */
// //   def addPart(x: Ob): State[ACSet, Part] = State(_.addPart(x, PropMap()))

// //   /** `State` wrapper around ACSet.setProp */
// //   def setProp(p: Part, f: Property, v: f.Value): State[ACSet, Unit] =
// //     State.modify(_.setProp(p, f, v))

// //   /** `State` wrapper around ACSet.remSubpart */
// //   def remSubpart(p: Part, f: Property): State[ACSet, Unit] =
// //     State.modify(_.remSubpart(p, f))

// //   /** `State` wrapper around ACSet.remPart */
// //   def remPart(p: Part): State[ACSet, Unit] = State.modify(_.remPart(p))

// //   /** `State` wrapper around ACSet.remParts */
// //   def remParts(ps: Seq[Part]): State[ACSet, Unit] = State.modify(_.remParts(ps))

// //   /** `State` wrapper around ACSet.moveFront */
// //   def moveFront(p: Part): State[ACSet, Unit] = State.modify(_.moveFront(p))

// //   /** Returns a lens into the value of the property `f` for part `x` */
// //   def subpartLens(f: Property, x: Part) =
// //     Lens[ACSet, f.Value](_.subpart(f, x))(y => s => s.setProp(x, f, y))
// // }




// // case class AddPartMsg(ob:Ob,props:PropMap = PropMap()) extends Message[ACSet] {
// //   def execute(a:ACSet) = a.addPart(ob,props)._1
// // }

// // case class RemovePartMsg(part:Part) extends Message[ACSet] {
// //   def execute(a:ACSet) = a.remPart(part)
// // }

// // case class SetSubpartMsg(part:Part,prop:Property)(v:prop.Value) extends Message[ACSet] {
// //   def execute(a:ACSet) = a.setProp(part,prop,v)
// // }

// // case class RemoveSubpartMsg(part:Part,prop:Property) extends Message[ACSet] {
// //   def execute(a:ACSet) = a.remSubpart(part,prop)
// // }

