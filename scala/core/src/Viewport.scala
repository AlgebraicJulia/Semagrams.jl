package semagrams

import com.raquo.laminar.api.L._
import semagrams.util._

/** A bit of state storing the current affine transform of a viewport.
  *
  * Currently unused.
  *
  * @todo
  *   use this for zooming/translation
  *
  * @todo
  *   maybe just use a `Var[Transform]` directly?
  */
case class TransformState(m: Var[Transform])

object TransformState {

  /** Construct a new TransformState with the identity transform */
  def apply() = new TransformState(Var(Transform.identity))
}

/** A semagrams app is split into several viewports.
  *
  * The reason for this is that we might want to apply different affine
  * transformations to different parts of the app. For instance, we might have
  * UI elements in one viewport, and the main acset in another viewport, so that
  * we can zoom out on the main acset without making all of the buttons tiny.
  *
  * Currently transforms are unimplemented; however this was just the result of
  * an incomplete refactor, and it should be straightforward to add them back.
  */
trait Viewport {

  /** The transform of the viewport; this will be able to be modified in order
    * to zoom/pan
    */
  val transform: TransformState

  /** The entities contained in the viewport. */
  val entities: Signal[EntityCollection]

  /** The root element of the viewport, which should be mounted on the root
    * element of the Semagram. Typically a `<g>`.
    */
  val elt: SvgElement

}

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

/** A basic implementation of Viewport that extracts entities using a sequence
  * of [[EntitySources]]s from a signal of type `A`.
  *
  * Later elements of `entitySources` have access to the entities, sprites and
  * acsets extracted by previous elements.
  */
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

/** Utility methods associated with Viewports */
object Viewport {

  /** Build a signal of a sequence of SvgElements from an EntityCollection.
    *
    * This behaves similarly to `split` from Laminar in that it caches the
    * rendered `SvgElement` for an [[Entity]], but passes into the rendering
    * method a `Signal` of updated ACSet data associated to that `Entity`.
    *
    * @todo
    *   remove dollar signs
    */
  def render($m: Signal[EntityCollection]): Signal[Seq[SvgElement]] = {
    val $em = $m.map(_.em)
    val $svgMap =
      $em.foldLeft(m => updateRendered($em)(Map[Entity, SvgElement](), m))(
        updateRendered($em)
      )
    $m.combineWith($svgMap).map((m, svgMap) => m.ordering.map(svgMap))
  }

  /** Internal method for [[render]] */
  private def updateRendered(
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
