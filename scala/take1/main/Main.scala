package semagrams

import semagrams.Semagrams._
import semagrams.Params._
import semagrams.ACSets._
import semagrams.Bridge._
import semagrams.Config

import com.raquo.laminar.api.L._
import org.scalajs.dom
import scala.collection.immutable._

object Main {
  val GraphSchema = Schema(
    Vector("E", "V"),
    Vector("src", "tgt"),
    Vector(),
    Vector(),
    HashMap("src" -> "E", "tgt" -> "E"),
    HashMap("src" -> "V", "tgt" -> "V")
  )

  def main(args: Array[String]): Unit = {
    val init_acset = ACSet[EntityParams](GraphSchema)

    val config = Config(
      sprite_radius = 20,
      sprite_default_fill = "white",
      sprite_hovered_fill = "lightgrey",
      sprite_stroke = "black",
      height = 400,
      width = 400)

    val appDiv = createBridge(init_acset, config)

    render(dom.document.querySelector("#appContainer"), appDiv)
  }
}
