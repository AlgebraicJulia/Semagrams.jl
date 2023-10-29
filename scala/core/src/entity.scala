package semagrams

import semagrams.util._
import semagrams.acsets.abstr._
import com.raquo.laminar.api.L._

/** A runtime-introspectable type tag for an Entity. */
trait EntityType

/** A reference to an logically distinct part of the Semagram, for instance a
  * vertex, an edge, a ui element, etc.
  */
trait Entity {
  val id: UUID// = UUID()
  /** We use a type tag so that we can compare the types of entities at runtime
    * without having to mess with scala introspection. Additionally, different
    * entities might have the same type, but different EntityTypes, a notable
    * example of this is [[Part]]s.
    */
  val ty: EntityType

  /** Construct a subentity of this entity
    */
  def extend(sub: Entity): Entity = SubEntity(this, sub)
}

/** An entity that is logically a part of the parent
  *
  * @todo
  *   I'm not sure we really use this; is this necessary?
  */
case class SubEntity(parent: Entity, child: Entity) extends Entity {
  val ty = SubEntityType(parent.ty, child.ty)
  val id = UUID("SubEntity")
}

/** The [[EntityType]] for [[SubEntity]]s */
case class SubEntityType(parentTy: EntityType, childTy: EntityType)
    extends EntityType

/** An entity for the background of the Semagrams app.
  *
  * Used as the source entity for click events on the background.
  */
val backgroundPart = Part(BackgroundOb)

/** The [[EntityType]] for Background */
object BackgroundOb extends Ob:
  def generators = Seq()
  def label = "BackgroundOb"

/** A map associating a [[Sprite[D]]] and an [[ACSet]] to the entities alive in the
  * Semagram.
  *
  * ACSets have properties associated to their roots, but also subparts, both of
  * which might be used by the [[Sprite[D]]].
  */
type EntityMap[D] = Map[Part, (Sprite[D], D)]

object EntityMap {

  /** Construct a new empty [[EntityMap]] */
  def apply[D:PartData](): EntityMap[D] = 
    Map[Part, (Sprite[D], D)]()
}

/** A wrapper around a function which extracts a sequence of entity, sprite,
  * acset tuples from data of type `A` and an [[EntityMap]] with
  * previously-extracted entities. The reason we have both something of type `A`
  * and an [[EntityMap]] is that we might want to query, for instance, the
  * boundary of a previously extracted vertex while constructing an edge.
  */
case class EntitySource[D:PartData,A:ACSetWithData[D]](
    entities: (A, EntityMap[D]) => Seq[(Part, Sprite[D], D)]
) {

  /** Use the EntitySource to add entities with their sprites/acsets to an
    * EntityMap
    */
  def addEntities(a: A, em: EntityMap[D]) = {
    em ++ entities(a, em).map((part, spr, data) => (part, (spr, data))).toMap
  }

  /** Construct a new [[EntitySource]] which adds the properties in `props` to
    * every acset produced by this [[EntitySource]].
    */
  def withProps(props: PropMap) = EntitySource[D,A](
    entities(_, _).map((part, spr, data) => (part, spr, data.setProps(props)))
  )

  def withSoftProps(props: PropMap) = EntitySource[D,A](
    entities(_, _).map((part, spr, data) => (part, spr, data.softSetProps(props)))
  )

  /** Construct a new [[EntitySource]] which uses `f` to make new properties to
    * add to every acset produced by this [[EntitySource]].
    */
  def addPropsBy(f: (Part, D, EntityMap[D]) => PropMap) =
    EntitySource[D,A]((a, em) =>
      entities(a, em).map((part, spr, data) => (part, spr, data.setProps(f(part, data, em))))
    )

  def softAddPropsBy(f: (Part, D, EntityMap[D]) => PropMap) =
    EntitySource[D,A]((a, em) =>
      entities(a, em).map((part, spr, data) => (part, spr, data.softSetProps(f(part, data, em))))
    )

  def addDataBy(f: (Part, D, EntityMap[D]) => D) =
    EntitySource[D,A]((a, em) =>
      entities(a, em).map((part, spr, data) => (part, spr, data.merge(f(part, data, em))))
    )

  def softAddDataBy(f: (Part, D, EntityMap[D]) => D) =
    EntitySource[D,A]((a, em) =>
      entities(a, em).map((part, spr, data) => (part, spr, f(part,data,em).merge(data)))
    )

  /** Construct a new [[EntitySource]] by using `f` to update every entity and
    * acset produced by this [[EntitySource]], while keeping the sprites the
    * same.
    */
  def updateEntities(f: (Part, D) => (Part, D)) =
    EntitySource[D,A]( (a, em) =>
      entities(a, em).map{ (part, spr, data) => 
        val (newpart, newdata) = f(part, data)
        (newpart, spr, newdata)
      }
    )
  
}
