package semagrams

import semagrams.ACSets._
import semagrams.Params._
import semagrams.Config

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._

object Sprite {
  enum SpriteEvent:
    case MouseOver(e: Entity)
    case MouseOut(e: Entity)

  case class SpriteConfig(
    events: L.Observer[SpriteEvent],
    global_config: Config
  )

  def createSprite(config: SpriteConfig)(
    entity: Entity,
    init: (Entity, EntityParams),
    updateSignal: L.Signal[(Entity, EntityParams)]
  ) = {
    val hovered = L.Var(false)
    circle(
      stroke := config.global_config.sprite_stroke,
      fill <-- hovered.signal.map(if _
                                    then config.global_config.sprite_hovered_fill
                                    else config.global_config.sprite_default_fill),
      cx <-- updateSignal.map(_._2.x.toString),
      cy <-- updateSignal.map(_._2.y.toString),
      r := config.global_config.sprite_radius.toString,
      L.onMouseOver --> config.events.contramap(ev => SpriteEvent.MouseOver(entity)),
      L.onMouseOut --> config.events.contramap(ev => SpriteEvent.MouseOut(entity)),
      L.onMouseOver --> hovered.writer.contramap(_ => true),
      L.onMouseOut --> hovered.writer.contramap(_ => false)
    )
  }
}

/**
 * Sprite design:
 * 
 * For each entity type, there must be a sprite schema.
 * 
 * This sprite schema contains the following things
 * - A description of the parameters for that sprite
 * - A description of the computation graph for those parameters
 *   - Handles
 *     - Given mouse position, update the parameters
 *   - Layout engines
 *     - Given parameters of related entities, update the parameters
 * - A function that takes in the Var of parameters and a Signal of attributes
 * and returns a reactive SVG, including handles
 * - A constructor that produces
 *   - A Signal of parameters
 *   - The reactive SVG
 * 
 * We store the Signal and the SVG in the ACSet itself, and they are used for
 * displaying the ACSet.  This is essentially a home-grown implementation of
 * split.
 * 
 * Problem #1: the signals for the layout engines will depend in general on the
 * Signal of the ACSet itself.  This could potentially cause a loop.
 * 
 * Problem #2: the inputs to a parameter Signal will need to change dynamically
 * based on the combinatorics of the ACSet. Therefore, I believe that we may need
 * a subscription manager.
 * 
 * Problem #3: it seems like there is an artificial distinction between
 * parameters and attributes. Both should be able to be manipulated by handles,
 * and both are used in visual display of the ACSet. We could take advantage of
 * this somehow. Perhaps to create a Semagram, we create an ACSet with some
 * additional attributes, and then export via data migration.
 * 
 * So it appears what we really need is a reactive ACSet.
 * 
 * A reactive ACSet is an ACSet along with a computation graph for its
 * attributes that depends on the combinatorics of the ACSet itself.
 * 
 * All of the attributes, including parameters, need to be saved for undo/redo.
 * 
 * Therefore, we need all of the attributes to be stored "cold" along with the
 * ACSet.
 * 
 * Also, the ACSet should be fully serializable.
 * 
 * Thus, we need to implement our own version of reactivity. This is probably
 * for the best.
 */
