package semagrams.sprites

import semagrams._
import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.nodes.ReactiveSvgElement
import semagrams.util.Complex

type PropMaps = Map[Entity, PropMap]

case class SpriteMaker[State](
    sprite: Sprite,
    extractor: (State, PropMaps) => List[(Entity, PropMap)],
    middleware: Middleware
)

class SpriteMaps[State](
    $state: Signal[State],
    spriteMakers: List[SpriteMaker[State]]
) {
  val $propMaps: Signal[(List[PropMaps], PropMaps)] = $state.map(state => {
    spriteMakers.foldLeft((List[PropMaps](), Map[Entity, PropMap]()))(
      (tup, spriteMaker) => {
        val (propMapses, propMaps) = tup
        val newSprites = spriteMaker
          .extractor(state, propMaps)
          .map(
            { case (ent, propMap) =>
              (ent, spriteMaker.middleware.updateProps(ent, propMap))
            }
          )
          .toMap
        (newSprites :: propMapses, newSprites ++ propMaps)
      }
    )
  }).map({ case (propMapses, propMaps) => (propMapses.reverse, propMaps) })

  def updateSprites(
      renderedSpritesList: List[Map[Entity, RenderedSprite]],
      propMapses: List[PropMaps]
  ): List[Map[Entity, RenderedSprite]] = {
    renderedSpritesList
      .zip(propMapses)
      .zip(spriteMakers)
      .map(
        {
          case ((renderedSprites, propMaps), spriteMaker) => {
            val added = propMaps.keySet -- renderedSprites.keySet
            val removed = renderedSprites.keySet -- propMaps.keySet
            val newRenderedSprites = added
              .map(ent => {
                val propMap = propMaps(ent)
                val propMapStream =
                  $propMaps.changes.map(_._2.get(ent)).collect { case Some(x) =>
                    x
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

  val $renderedSprites = $propMaps.map(_._1).foldLeft(propMapses =>
    updateSprites(spriteMakers.map(_ => Map()), propMapses)
  )(updateSprites)

  def attach =
    children <-- $renderedSprites.map(
      _.map(_.values.map(_.root).toList).flatten
    )
}
