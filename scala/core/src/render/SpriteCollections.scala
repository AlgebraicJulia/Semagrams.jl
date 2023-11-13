package semagrams.rendering

import semagrams.acsets._
import semagrams.util.Complex

/** A collection of entities and sprites
  *
  * @param spritemap
  *   an [[SpriteMap]] mapping entities to [[Sprite]]s and [[ACSet]]s
  *
  * @param ordering
  *   the order in which the sprites should be placed in the window. This
  *   matters for figuring out which sprite should be on top in the case of
  *   overlaps.
  */
case class SpriteCollection[Model,D:PartData](spritemap: SpriteMap[D],ordering: Seq[Part]):

  /** Construct a new [[SpriteCollection]] by adding the entities coming from
    * `source`
    */
  def addSource(m: Model, source: SpriteSource[Model,D]): SpriteCollection[Model,D] = {
    val sprs = source.makeSprites(m, spritemap)
      .filter{ case _ -> (sprite,data) => data.hasProps(sprite.requiredProps)}
    SpriteCollection(spritemap ++ sprs.toMap, ordering ++ sprs.map(_._1))
  }

  
object SpriteCollection:
  /** Construct a new empty [[SpriteCollection]] */
  def apply[Model,D:PartData]() = new SpriteCollection[Model,D](SpriteMap(), Seq())


object EntityCollector:
  def collect[Model,D:PartData](
      m: Model,
      sources: Seq[SpriteSource[Model,D]]
  ): Seq[(Part, (Sprite[D],D))] =
    val collection = sources.foldLeft(SpriteCollection[Model,D]())((coll, source) =>
      coll.addSource(m, source)
    )
    collection.ordering.flatMap(part =>
      collection.spritemap.get(part).map((spr,data) =>
        part -> (spr,data)  
      )  
    )



/** A partial function from `Part`s to sequences of (sprite,initialization) pairs.
  * 
  * In general, a part may generate several elements, e.g., a box in a wiring diagram
  * might also generate its ports. 
  */
  
type SpriteMap[D] = Map[Part, (Sprite[D],D)]

object SpriteMap:
  /** Construct m new empty [[SpriteMap]] */
  def apply[D:PartData](): SpriteMap[D] = 
    Map[Part, (Sprite[D], D)]()

object SpriteMapExtension {
  extension [D](spritemap:SpriteMap[D])

    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `spritemap`
    */
    def findCenter(p:Part) =
      spritemap.get(p).flatMap( (spr,data) =>
        spr.center(data,Seq())  
      )
    /** Find the center of the sprite corresponding to `p`, 
      * by looking up the sprite/data in `spritemap`
    */
    def findCenter(p:Part,subparts:Seq[Part]) =
      spritemap.get(p).flatMap( (spr,data) =>
        spr.center(data,subparts)  
      )


    /** Find the point on the boundary in direction `dir` of the sprite
      * corresponding to `p`, by looking up the sprite/data in `m`
      */
    def findBoundary(p: Part,dir: Complex) = 
      spritemap.get(p).flatMap( (spr,data) =>
        spr.boundaryPt(data,dir,Seq())
      )
    
    def findBoundary(p: Part,dir: Complex,subparts:Seq[Part]) = 
      spritemap.get(p).flatMap( (spr,data) =>
        spr.boundaryPt(data,dir,subparts)
      )

}
export SpriteMapExtension._




/** Model wrapper around m function which extracts m sequence of entity, sprite,
  * acset tuples from data of type `Model` and an [[SpriteMap]] with
  * previously-extracted makeSprites. The reason we have both something of type `Model`
  * and an [[SpriteMap]] is that we might want to query, for instance, the
  * boundary of m previously extracted vertex while constructing an edge.
  */
case class SpriteSource[Model,D:PartData](
    makeSprites: (Model, SpriteMap[D]) => Seq[(Part, (Sprite[D], D))]
) {
  def mapData(f:D => D) = SpriteSource[Model,D](
    makeSprites(_,_).map{ case part -> (spr,init) => part -> (spr,f(init))}
  )

  /** Use the SpriteSource to add makeSprites with their sprites/acsets to an
    * SpriteMap
    */
  def addEntities(m: Model, spritemap: SpriteMap[D]) = {
    spritemap ++ makeSprites(m, spritemap).toMap
  }

  /** Construct m new [[SpriteSource]] which adds the properties in `props` to
    * every acset produced by this [[SpriteSource]].
    */
  def withProps(props: PropMap) = mapData(_.setProps(props))

  def withSoftProps(props: PropMap) = mapData(_.softSetProps(props))

  /** Construct m new [[SpriteSource]] which uses `f` to make new properties to
    * add to every acset produced by this [[SpriteSource]].
    */
  def addPropsBy(f: (Part,SpriteMap[D],D) => PropMap) = SpriteSource[Model,D](
    (model,spritemap) => makeSprites(model,spritemap).map{ 
      case part -> (spr,init) => part -> (spr,init.setProps(f(part,spritemap,init)))
    }
  )

  def softAddPropsBy(f: (Part,SpriteMap[D],D) => PropMap) = SpriteSource[Model,D](
    (m, spritemap) => makeSprites(m, spritemap).map {
      case part -> (spr, init) => part -> (spr, init.softSetProps(f(part,spritemap,init)))
    }
  )

  // def addDataBy(f: (Part, D, SpriteMap[D]) => D) =
  //   SpriteSource[Model,D]((m, spritemap) =>
  //     makeSprites(m, spritemap).map((part, spr, data) => (part, spr, data.merge(f(part, data, spritemap))))
  //   )

  // def softAddDataBy(f: (Part, D, SpriteMap[D]) => D) =
  //   SpriteSource[Model,D]((m, spritemap) =>
  //     makeSprites(m, spritemap).map((part, spr, data) => (part, spr, f(part,data,spritemap).merge(data)))
  //   )

  /** Construct m new [[SpriteSource]] by using `f` to update every entity and
    * acset produced by this [[SpriteSource]], while keeping the sprites the
    * same.
    */
  // def updateEntities(f: (Part, D) => (Part, D)) =
  //   SpriteSource[Model,D]( (m, spritemap) =>
  //     makeSprites(m, spritemap).map{ (part, spr, data) => 
  //       val (newpart, newdata) = f(part, data)
  //       (newpart, spr, newdata)
  //     }
  //   )
  
}
