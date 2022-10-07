package org.algebraicjulia.semagrams.util

import com.raquo.domtypes.generic.Modifier

class CustomModifier[-El](f: El => Unit) extends Modifier[El] {
  override def apply(element: El) = f(element)
}
