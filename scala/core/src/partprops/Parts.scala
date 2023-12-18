package semagrams.partprops

import upickle.default._

import semagrams.util._
import semagrams.acsets.{Elt, Ob}

case class Part(id: UID, ob: Ob):

  override def toString = if ob.label == ""
  then "Anon-" + id.rand
  else ob.label + "-" + id.rand

  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.label + id.toString

object Part:
  def apply(x: Ob) = new Part(UID("Part"), x)
  def apply(id: UID, x: Ob) = new Part(id, x)
