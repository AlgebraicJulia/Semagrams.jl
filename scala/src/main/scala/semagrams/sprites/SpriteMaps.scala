package semagrams.sprites

import semagrams._
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util.Complex

type Sprites = Map[Entity, (Sprite, PropMap)]

case class SpriteMaker[State](
    sprite: Sprite,
    extractor: (State, Sprites) => List[(Entity, PropMap)],
    middleware: Middleware
)

class SpriteMaps[State](
    $state: Signal[State],
    spriteMakers: List[SpriteMaker[State]]
) {
  val $sprites: Signal[(List[Sprites], Sprites)] = $state.map(state => {
    spriteMakers.foldLeft((List[Sprites](), Map[Entity, (Sprite, PropMap)]()))(
      (tup, spriteMaker) => {
        val (spriteses, sprites) = tup
        val newSprites = spriteMaker
          .extractor(state, sprites)
          .map(
            { case (ent, propMap) =>
              (ent, (spriteMaker.sprite, spriteMaker.middleware.updateProps(ent, propMap)))
            }
          )
          .toMap
        (newSprites :: spriteses, newSprites ++ sprites)
      }
    )
  }).map({ case (propMapses, propMaps) => (propMapses.reverse, propMaps) })

  def updateSprites(
      renderedSpritesList: List[Map[Entity, RenderedSprite]],
      spriteses: List[Sprites]
  ): List[Map[Entity, RenderedSprite]] = {
    renderedSpritesList
      .zip(spriteses)
      .zip(spriteMakers)
      .map(
        {
          case ((renderedSprites, sprites), spriteMaker) => {
            val added = sprites.keySet -- renderedSprites.keySet
            val removed = renderedSprites.keySet -- sprites.keySet
            val newRenderedSprites = added
              .map(ent => {
                val (_, propMap) = sprites(ent)
                val propMapStream =
                  $sprites.changes.map(_._2.get(ent)).collect { case Some(x) =>
                    x._2
                  }
                val $propMap = propMapStream.toSignal(propMap)
                val newRS = spriteMaker.sprite.present(
                  ent,
                  propMap,
                  spriteMaker.middleware.updatePropsS(ent, $propMap)
                )
                (ent, spriteMaker.middleware.modifyRendered(ent, newRS))
              })
              .toMap
            (renderedSprites ++ newRenderedSprites) -- removed
          }
        }
      )
  }

  val $renderedSprites = $sprites.map(_._1).foldLeft(propMapses =>
    updateSprites(spriteMakers.map(_ => Map()), propMapses)
  )(updateSprites)

  def attach =
    children <-- $renderedSprites.map(
      _.map(_.values.map(_.root).toList).flatten
    )
}
