package acsets

import java.util.UUID
import scala.collection.mutable

object Patches {

  opaque type PatchTag = UUID
  opaque type LID = Int
  opaque type EID = Int

  case class Ident(
    tag: PatchTag,
    lid: LID,
    name: Option[String]
  )

  case class Binding[T](
    name: Option[String],
    ty: T
  )

  case class Definition[D](
    operand: Ident,
    operation: Ident,
    result: D
  )

  case class PatchContent[T, D](
    val parents: Seq[PatchTag],
    val bindings: Seq[Binding[T]],
    val definitions: Seq[Definition[D]],
    val dead: Seq[Ident]
  )

  class Patch[T, D] private (
    val tag: PatchTag,
    val content: PatchContent[T, D],
    val lookup: Map[String, LID]
  ) {
    def apply(name: String): Ident = {
      val lid = lookup(name)
      Ident(tag, lid, Some(name))
    }
  }
}
