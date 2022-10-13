package semagrams.petri

import com.raquo.laminar.api.L

val CssSettings = scalacss.devOrProdDefaults
import CssSettings._

object PS extends StyleSheet.Inline {
  import dsl._

  val flex1 = mixin(
    display.flex,
    flex := "1"
  )

  val row = style(
    flex1,
    flexDirection.row,
  )

  val col = style(
    flex1,
    flexDirection.column
  )
}

val PSrendered = PS.render[String]
