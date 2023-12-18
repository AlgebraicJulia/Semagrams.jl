package semagrams.rendering

import semagrams.util._
import upickle.default._
import semagrams.acsets._
import semagrams.partprops.Part

trait EntityTag:
  type Key
  val contextId: UID
  val key: Key

case object BackgroundTag extends EntityTag {
  type Key = Unit

  override val contextId: UID = UID("Background")

  override val key: Key = ()

}

sealed trait PartTag extends EntityTag with Ordered[PartTag]:
  def keyPart: Part

  def compare(that: PartTag) =
    val ctxtOrder = contextId.compare(that.contextId)
    val partOrder = keyPart.id.compare(that.keyPart.id)
    ctxtOrder max (ctxtOrder min partOrder)

sealed trait EdgeTag extends PartTag:
  def srcTag: ObTag
  def tgtTag: Option[ObTag]

case class ObTag(part: Part, contextId: UID) extends PartTag:
  type Key = Part
  val key = part
  def keyPart: Part = key

  val ob = part.ob

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
