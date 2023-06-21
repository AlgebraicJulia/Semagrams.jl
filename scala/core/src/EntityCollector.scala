package semagrams

import semagrams.acsets._

/** A collection of entities and sprites
  *
  * @param em
  *   an [[EntityMap]] mapping entities to [[Sprite]]s and [[ACSet]]s
  *
  * @param ordering
  *   the order in which the sprites should be placed in the window. This
  *   matters for figuring out which sprite should be on top in the case of
  *   overlaps.
  */
case class EntityCollection(
    em: EntityMap,
    ordering: Seq[Entity]
) {

  /** Construct a new [[EntityCollection]] by adding the entities coming from
    * `source`
    */
  def addSource[A](a: A, source: EntitySource[A]): EntityCollection = {
    val xs = source.entities(a, em)
    val xsMap = xs.map((e, s, p) => (e, (s, p))).toMap
    EntityCollection(em ++ xsMap, ordering ++ xs.map(_._1))
  }
}

object EntityCollection {

  /** Construct a new empty [[EntityCollection]] */
  def apply() = new EntityCollection(EntityMap(), Seq())
}

object EntityCollector {
  def collect[A](a: A, sources: Seq[EntitySource[A]]): Seq[(Entity, ACSet, Sprite)] = {
    val collection = sources.foldLeft(EntityCollection())((ec, source) => ec.addSource(a, source))
    collection.ordering.map(
      ent => {
        val (sprite, acset) = collection.em(ent)
        (ent, acset, sprite)
      }
    )
  }
}
