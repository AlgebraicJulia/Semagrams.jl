package semagrams

import com.raquo.laminar.api.L._

import semagrams.acsets._

trait Semagram {
  type Model

  /** Computes dynamically some layout properties.
    *
    * Note that the type signature of this assumes that the layout can be stored
    * in the same type as the input. This is a priori not the case, but is the
    * case for the current way we do layout with ACSets. For the sake of
    * simplicity I'm not going to make two types that are going to be always
    * instantiated to the same type for the forseeable future, but I'm making
    * this note so that we know what to change if we need to do so.
    */
  def layout(m: Model): Model

  /** Produces a list of sprites to be rendered.
    *
    * This is a pure function, so it can be tested.
    */
  def produceSprites(m: Model, eventWriter: Observer[Event]): Seq[(Entity, ACSet, Sprite)]

  /** Creates the svg element ready to be inserted into a larger app. */
  def apply(mSig: Signal[Model], eventWriter: Observer[Event]): SvgElement = {
    svg.svg(
      children <-- mSig.map(produceSprites(_, eventWriter)).split(_._1)(
        (ent, init, updates) => {
          val (_, initAcset, sprite) = init
          sprite.present(ent, initAcset, updates.map(_._2), (_,_) => ())
        }
      )
    )
  }
}
