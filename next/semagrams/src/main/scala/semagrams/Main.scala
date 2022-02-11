package semagrams

import semagrams.examples.StaticBoxes
import semagrams.sprites._
import semagrams.util.Complex
import org.scalajs.dom
import com.raquo.laminar.api.L._

object Main {
  val boxes = Map(Entity("box", 1) -> BoxData(Complex(50,50), Complex(50,50)))

  def main(args: Array[String]): Unit = {
    val appDiv = new StaticBoxes().start(boxes)
    render(dom.document.querySelector("#appContainer"), appDiv)
  }
}
