package semagrams

import com.raquo.laminar.api.L._

trait Controller {
  def apply(es: EditorState, elt: SvgElement): Unit
}
