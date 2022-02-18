package semagrams

import semagrams.examples._
import semagrams.sprites._
import semagrams.util.Complex
import org.scalajs.dom
import com.raquo.laminar.api.L._

object Main {
  val boxes = Boxes()

  def main(args: Array[String]): Unit = {
    val appDiv = StaticBoxes(Var(boxes)).present()
    render(dom.document.querySelector("#appContainer"), appDiv)
  }
}
