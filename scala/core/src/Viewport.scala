package semagrams

import com.raquo.laminar.api.L._

case class TransformState()

class Viewport(entitySources: Seq[EntitySource]) {
  val transform = TransformState()

  val entities = entitySources.foldLeft[Signal[EntityMap]](
    Val(EntityMap())
  )(($m, es) => es.addEntities($m))

  val elt = svg.g(
    children <-- Viewport.render(entities)
  )
}

object Viewport {
  def render($m: Signal[EntityMap]): Signal[Seq[SvgElement]] = {
    $m.foldLeft(m => updateRendered($m)(Map[Entity, SvgElement](), m))(
      updateRendered($m)
    ).map(_.values.toSeq)
  }

  def updateRendered(
      $m: Signal[EntityMap]
  )(r: Map[Entity, SvgElement], m: EntityMap): Map[Entity, SvgElement] = {
    val added = m.keySet -- r.keySet
    val removed = r.keySet -- m.keySet
    val newElements = added
      .map(ent => {
        val (sprite, propMap) = m(ent)
        // Only update propMap when it exists in $m, don't error when it's deleted
        val propMapStream =
          $m.changes.map(_.get(ent)).collect { case Some(x) => x._2 }
        val $propMap = propMapStream.toSignal(propMap)
        (ent, sprite.present(ent, propMap, $propMap))
      })
      .toMap
    (r -- removed) ++ newElements
  }
}
