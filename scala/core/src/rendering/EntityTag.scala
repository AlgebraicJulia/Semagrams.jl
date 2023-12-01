package semagrams.rendering

import semagrams.util._
import upickle.default._
import semagrams.acsets._
import semagrams.partprops.Part

/** Model runtime-introspectable type tag for an Entity. */
// trait EntityType

/** Model reference to an logically distinct part of the Semagram, for instance
  * m vertex, an edge, m ui element, etc.
  */
// trait Entity:
//   val tag: EntityTag

trait EntityTag:
  type Key
  val contextId: UID
  val key: Key

case object BackgroundTag extends EntityTag {
  type Key = Unit

  override val contextId: UID = UID("Background")

  override val key: Key = ()

}
// object Background extends Entity {
//   val tag = BackgroundTag
//   override def toString = "Background"
// }

sealed trait PartTag extends EntityTag with Ordered[PartTag]:
  def keyPart: Part

  def compare(that: PartTag) =
    val ctxtOrder = contextId.compare(that.contextId)
    val partOrder = keyPart.id.compare(that.keyPart.id)
    ctxtOrder max (ctxtOrder min partOrder)

sealed trait EdgeTag extends PartTag:
  def srcTag: ObTag
  def tgtTag: Option[ObTag]

case class ObTag(part: Part, contextId: UID) extends PartTag derives ReadWriter:
  type Key = Part
  val key = part
  def keyPart: Part = key

  val ob = part.ob

// object ObTag:
//   implicit val partrw: ReadWriter[Part] = Part.rw
//   implicit val rw: ReadWriter[ObTag] = macroRW

case class HomTag(
    contextId: UID,
    hom: Hom[_],
    src: ObTag,
    tgt: Option[ObTag]
) extends EdgeTag:
  type Key = (ObTag, Option[ObTag])
  val key = src -> tgt
  def keyPart = src.keyPart

  def srcTag = src
  def tgtTag = tgt

case class SpanTag(
    contextId: UID,
    span: Span[_],
    apex: Part,
    feet: (ObTag, Option[ObTag])
) extends EdgeTag:
  type Key = (Part, (ObTag, Option[ObTag]))
  val key = apex -> feet
  def keyPart = apex

  val dom = span.dom
  val (srcTag, tgtTag) = feet
