package semagrams

import com.raquo.laminar.api.L._

/** Controllers manage global state. They have an `apply` method which hooks
  * them up to whatever they need in the [[EditorState]], and also adds the
  * right hooks to the main element.
  */
trait Controller {
  def apply(es: EditorState, elt: SvgElement): Unit
}
