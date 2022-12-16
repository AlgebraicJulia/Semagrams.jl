package semagrams.ui

import semagrams._
import com.raquo.laminar.api.L._

case class UIState(
  sprites: Var[Vector[(Entity, Sprite, PropMap)]]
) {
  def setEntity(e: Entity, s: Sprite, p: PropMap) = {
    sprites.update(
      entities => {
        val i = entities.indexWhere(_._1 == e)
        if ( i != -1 ) {
          entities.updated(i, (e, s, p))
        } else {
          entities :+ (e, s, p)
        }
      }
    )
  }

  def remEntity(e: Entity) = {
    sprites.update(_.filterNot(_._1 == e))
  }
}
