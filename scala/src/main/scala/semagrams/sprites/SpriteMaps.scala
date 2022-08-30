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
  private[this] val $renderedSpritesVar
      : Var[List[Map[Entity, RenderedSprite]]] = Var(
    spriteMakers.map(_ => Map())
  )

  val $renderedSprites: Signal[List[Map[Entity, RenderedSprite]]] =
    $renderedSpritesVar.signal

  val $propMaps: Signal[(List[PropMaps], PropMaps)] = $state.map(state => {
    spriteMakers.foldLeft((List[PropMaps](), Map[Entity, PropMap]()))(
      (tup, spriteMaker) => {
        val propMaps = tup._2
        val newSprites = spriteMaker
          .extractor(state, propMaps)
          .map(
            { case (ent, propMap) =>
              (ent, spriteMaker.middleware.updateProps(ent, propMap))
            }
          )
          .toMap
        (newSprites :: tup._1, propMaps ++ newSprites)
      }
    )
  }).map({ case (propMapsList, propMaps) => (propMapsList.reverse, propMaps) })

  def updateSprites(
      renderedSpritesList: List[Map[Entity, RenderedSprite]],
      propMapsList: List[PropMaps]
  ): List[Map[Entity, RenderedSprite]] = {
    renderedSpritesList
      .zip(propMapsList)
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

  def attach =
    List(
      $propMaps.map(_._1) --> $renderedSpritesVar.updater(updateSprites),
      children <-- $renderedSprites.map(
        _.map(_.values.map(_.root).toList).flatten
      )
    )
}
