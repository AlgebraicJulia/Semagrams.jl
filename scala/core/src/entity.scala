package semagrams

import semagrams.acsets._
import com.raquo.laminar.api.L._

/** A runtime-introspectable type tag for an Entity. */
trait EntityType

/** A reference to an logically distinct part of the Semagram,
  * for instance a vertex, an edge, a ui element, etc.
  */
trait Entity {
  /** We use a type tag so that we can compare the types of entities
    * at runtime without having to mess with scala introspection. Additionally,
    * different entities might have the same type, but different EntityTypes,
    * a notable example of this is [[Part]]s.
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
}

/** The [[EntityType]] for [[SubEntity]]s */
case class SubEntityType(parentTy: EntityType, childTy: EntityType)
    extends EntityType

/** An entity for the background of the Semagrams app.
  * 
  * Used as the source entity for click events on the background.
  */
case class Background() extends Entity {
  val ty = Background
}

/** The [[EntityType]] for Background */
object Background extends EntityType

/** A map associating a [[Sprite]] and an [[ACSet]] to the entities alive in the
  * Semagram.
  * 
  * ACSets have properties associated to their roots, but also subparts, both of
  * which might be used by the [[Sprite]].
  */
type EntityMap = Map[Entity, (Sprite, ACSet)]

object EntityMap {
  /** Construct a new empty [[EntityMap]] */
  def apply(): EntityMap = Map[Entity, (Sprite, ACSet)]()
}

/** A wrapper around a function which extracts a sequence of entity, sprite,
  * acset tuples from data of type `A` and an [[EntityMap]] with
  * previously-extracted entities. The reason we have both something of type `A`
  * and an [[EntityMap]] is that we might want to query, for instance, the
  * boundary of a previously extracted vertex while constructing an edge.
  */
case class EntitySource[A](
    entities: (A, EntityMap) => Seq[(Entity, Sprite, ACSet)]
) {

  /** Use the EntitySource to add entities with their sprites/acsets to an
    * EntityMap */
  def addEntities(a: A, m: EntityMap) = {
    m ++ entities(a, m).map((e, s, p) => (e, (s, p))).toMap
  }

  /** Construct a new [[EntitySource]] which adds the properties in `props` to
    * every acset produced by this [[EntitySource]].
    */
  def withProps(props: PropMap) = EntitySource[A](
    entities(_, _).map((e, s, a) => (e, s, a.addProps(props)))
  )

  /** Construct a new [[EntitySource]] which uses `f` to make new properties to
    * add to every acset produced by this [[EntitySource]].
    */
  def addPropsBy(f: (Entity, ACSet, EntityMap) => PropMap) =
    EntitySource[A]((g, m) =>
      entities(g, m).map((e, s, a) => (e, s, a.addProps(f(e, a, m))))
    )

  /** Construct a new [[EntitySource]] by using `f` to update every entity and
    * acset produced by this [[EntitySource]], while keeping the sprites the
    * same.
    */
  def updateEntities(f: (Entity, ACSet) => (Entity, ACSet)) = {
    EntitySource[A]((g, m) =>
      entities(g, m).map((e, s, a) => { val (e1, a1) = f(e, a); (e1, s, a1) })
    )
  }
}

