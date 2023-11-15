package semagrams.rendering

import semagrams.{PartData,PropMap}
import semagrams.acsets.Part
import semagrams.util.{Complex,RGB}

/** A collection of entities and sprites
  *
  * @param entitymap
  *   an [[EntityMap]] mapping entities to [[Sprite]]s and [[ACSet]]s
  *
  * @param ordering
  *   the order in which the sprites should be placed in the window. This
  *   matters for figuring out which sprite should be on top in the case of
  *   overlaps.
  */
case class EntityCollection[Model,D:PartData](entitymap: EntityMap[D],ordering: Seq[Part]):

  /** Construct a new [[EntityCollection]] by adding the entities coming from
    * `source`
    */
  def addSource(m: Model, source: EntitySource[Model,D]): EntityCollection[Model,D] = {
    val sprs = source.makeSprites(m, entitymap)
      .filter{ case _ -> (sprite,data) => data.hasProps(sprite.requiredProps)}
    EntityCollection(entitymap ++ sprs.toMap, ordering ++ sprs.map(_._1))
  }

  
object EntityCollection:
  /** Construct a new empty [[EntityCollection]] */
  def apply[Model,D:PartData]() = new EntityCollection[Model,D](EntityMap(), Seq())


object EntityCollector:
  def collect[Model,D:PartData](
      m: Model,
      sources: Seq[EntitySource[Model,D]]
  ): Seq[(Part, (Sprite[D],D))] =
    val collection = sources.foldLeft(EntityCollection[Model,D]())((coll, source) =>
      coll.addSource(m, source)
    )
    collection.ordering.flatMap(part =>
      collection.entitymap.get(part).map((spr,data) =>
        part -> (spr,data)  
      )  
    )



/** A partial function from `Part`s to sequences of (sprite,initialization) pairs.
  * 
  * In general, a part may generate several elements, e.g., a box in a wiring diagram
  * might also generate its ports. 
  */
  
type EntityMap[D] = Map[Part, (Sprite[D],D)]

object EntityMap:
  /** Construct m new empty [[EntityMap]] */
  def apply[D:PartData](): EntityMap[D] = 
    Map[Part, (Sprite[D], D)]()

object EntityMapExtension {
  extension [D](entitymap:EntityMap[D])

    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `entitymap`
    */
    def findCenter(p:Part) =
      entitymap.get(p).flatMap( (spr,data) =>
        spr.center(data,Seq())  
      )
    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `entitymap`
    */
    def findCenter(p:Part,subparts:Seq[Part]) =
      entitymap.get(p).flatMap( (spr,data) =>
        spr.center(data,subparts)  
      )


    /** Find the point on the boundary in direction `dir` of the sprite
      * corresponding to `p`, by looking up the sprite/data in `m`
      */
    def findBoundary(p: Part,dir: Complex) = 
      entitymap.get(p).flatMap( (spr,data) =>
        spr.boundaryPt(data,dir,Seq())
      )
    
    def findBoundary(p: Part,dir: Complex,subparts:Seq[Part]) = 
      entitymap.get(p).flatMap( (spr,data) =>
        spr.boundaryPt(data,dir,subparts)
      )

}
export EntityMapExtension._




/** Model wrapper around m function which extracts m sequence of entity, sprite,
  * acset tuples from data of type `Model` and an [[EntityMap]] with
  * previously-extracted makeSprites. The reason we have both something of type `Model`
  * and an [[EntityMap]] is that we might want to query, for instance, the
  * boundary of m previously extracted vertex while constructing an edge.
  */
case class EntitySource[Model,D:PartData](
    makeSprites: (Model, EntityMap[D]) => Seq[(Part, (Sprite[D], D))]
) {
  def mapData(f:D => D) = EntitySource[Model,D](
    makeSprites(_,_).map{ case part -> (spr,init) => part -> (spr,f(init))}
  )

  /** Use the EntitySource to add makeSprites with their sprites/acsets to an
    * EntityMap
    */
  def addEntities(m: Model, entitymap: EntityMap[D]) = {
    entitymap ++ makeSprites(m, entitymap).toMap
  }

  /** Construct m new [[EntitySource]] which adds the properties in `props` to
    * every acset produced by this [[EntitySource]].
    */
  def withProps(props: PropMap) = mapData(_.setProps(props))

  def withSoftProps(props: PropMap) = mapData(_.softSetProps(props))

  /** Construct m new [[EntitySource]] which uses `f` to make new properties to
    * add to every acset produced by this [[EntitySource]].
    */
  def addPropsBy(f: (Part,EntityMap[D],D) => PropMap) = EntitySource[Model,D](
    (model,entitymap) => makeSprites(model,entitymap).map{ 
      case part -> (spr,init) => part -> (spr,init.setProps(f(part,entitymap,init)))
    }
  )

  def softAddPropsBy(f: (Part,EntityMap[D],D) => PropMap) = EntitySource[Model,D](
    (m, entitymap) => makeSprites(m, entitymap).map {
      case part -> (spr, init) => part -> (spr, init.softSetProps(f(part,entitymap,init)))
    }
  )

  
}
