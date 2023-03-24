package semagrams

import com.raquo.laminar.api.L._
import semagrams.util._

case class TransformState(
  m: Var[Transform]
)

object TransformState {
  def apply() = new TransformState(Var(Transform.identity))
}

trait Viewport {
  val transform: TransformState

  val entities: Signal[EntityCollection]

  val elt: SvgElement


}

case class EntityCollection(
    em: EntityMap,
    ordering: Seq[Entity]
) {
  def addSource[A](a: A, source: EntitySource[A]) = {
    val xs = source.entities(a, em)
    val xsMap = xs.map((e, s, p) => (e, (s, p))).toMap
    EntityCollection(em ++ xsMap, ordering ++ xs.map(_._1))
  }
}

object EntityCollection {
  def apply() = new EntityCollection(EntityMap(), Seq())
}

class EntitySourceViewport[A](
    state: Signal[A],
    entitySources: Seq[EntitySource[A]]
) extends Viewport {
  val transform = TransformState()

  val entities = state.map(a => {
    entitySources.foldLeft(EntityCollection())((c, source) =>
      c.addSource(a, source)
    )
  })

  val elt = svg.g(
    children <-- Viewport.render(entities)
  )
}

object Viewport {
  def render($m: Signal[EntityCollection]): Signal[Seq[SvgElement]] = {
    val $em = $m.map(_.em)
    val $svgMap =
      $em.foldLeft(m => updateRendered($em)(Map[Entity, SvgElement](), m))(
        updateRendered($em)
      )
    $m.combineWith($svgMap).map((m, svgMap) => m.ordering.map(svgMap))
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
        (ent, sprite.present(ent, propMap, $propMap, (ent, elt) => ()))
      })
      .toMap
    (r -- removed) ++ newElements
  }
}
