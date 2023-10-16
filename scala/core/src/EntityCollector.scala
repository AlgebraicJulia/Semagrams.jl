package semagrams

import semagrams.acsets.abstr._

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
case class EntityCollection[D:PartData,Model](
    em: EntityMap[D],
    ordering: Seq[Part]
) {

  /** Construct a new [[EntityCollection]] by adding the entities coming from
    * `source`
    */
  def addSource(m: Model, source: EntitySource[D,Model]): EntityCollection[D,Model] = {
    val xs = source.entities(m, em)
    val xsMap = xs.map((part, spr, data) => (part, (spr, data))).toMap
    EntityCollection(em ++ xsMap, ordering ++ xs.map(_._1))
  }
}

object EntityCollection {

  /** Construct a new empty [[EntityCollection]] */
  def apply[D:PartData,Model]() = new EntityCollection[D,Model](EntityMap(), Seq())
}

object EntityCollector {
  def collect[D:PartData,Model](
      m: Model,
      sources: Seq[EntitySource[D,Model]]
  ): Seq[(Part, D, Sprite[D])] = {
    val collection = sources.foldLeft(EntityCollection[D,Model]())((ec, source) =>
      ec.addSource(m, source)
    )
    collection.ordering.map(ent => {
      val (sprite, acset) = collection.em(ent)
      (ent, acset, sprite)
    })
  }
}
