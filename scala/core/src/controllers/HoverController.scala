package semagrams.controllers

import semagrams._
import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._

/** A bit of global state that keeps track of the entity that is currently
  * hovered.
  */
class HoverController() extends Controller {
  import HoverController.State

  val state = Var(State(None))

  /** Returns a list of binders that can be applied to an svg element in order
    * to make that svg element update the HoverController on mouseenter and
    * mouseleave events.
    *
    * Example use:
    *
    * {{{
    * svg.rect(
    *   hover.hoverable(ent)
    * )
    * }}}
    *
    * @param ent
    *   the Entity which should be recorded as being hovered
    */
  def hoverable(ent: Entity) = List(
    onMouseEnter --> state.updater((state, _) => state.hover(ent)),
    onMouseLeave --> state.updater((state, _) => state.leave(ent))
  )

  /** Returns a [[laminar.Signal]] that either has the state `caseHovered` or
    * `caseUnhovered` depending on whether `ent` is hovered or not.
    *
    * @param ent
    *   the entity we care about
    * @param caseHovered
    *   the value the signal should have when `ent` is hovered
    * @param caseUnhovered
    *   the value the signal should have when `ent` is not hovered
    */
  def switchState[A](ent: Entity, caseHovered: A, caseUnhovered: A): Signal[A] =
    state.signal.map(state =>
      if state.isHovered(ent) then caseHovered else caseUnhovered
    )

  /** Does nothing
    *
    * Required because [[HoverController]] is a [[Controller]], but we don't
    * actually need to hook up any part of the EditorState.
    */
  def apply(_es: EditorState, _elt: SvgElement) = {}
}

object HoverController {

  /** Constructs a new HoverController
    */
  def apply() = new HoverController()

  /** The state of the HoverController, which is contained in a Var in
    * [[HoverController]].
    *
    * @param state
    *   this is `Some(ent)` if `ent` is hovered, `None` otherwise
    */
  case class State(state: Option[Entity]) {

    /** Update the state to reflect that `ent` is hovered */
    def hover(ent: Entity) = State(Some(ent))

    /** Update the state to reflect that `ent` is no longer hovered */
    def leave(ent: Entity) = State(
      // Only unhover if we are actually leaving the entity that we were
      // hovering over in the first place.
      if Some(ent) == state
      then None
      else state
    )

    /** Returns whether `ent` is hovered */
    def isHovered(ent: Entity) = state == Some(ent)
  }
}
