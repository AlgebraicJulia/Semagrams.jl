package semagrams.util

// import utest._
import semagrams.util.Complex.im

import weaver._
import scala.math._
import cats.effect.IO

object ComplexSpec extends FunSuite {
  test("arithmetic") {
    val z1 = Complex(1, 2)
    val z2 = Complex(2, 3)
    expect.all(
      -z1 == Complex(-1, -2),
      z1 + z2 == Complex(3, 5),
      z1 * z2 == Complex(-4, 7),
      z1 - z2 == Complex(-1, -1),
      z1 / Complex(2, 0) == Complex(0.5, 1),
      z1 / Complex(0, 1) == z1 * Complex(0, -1)
    )
  }

  test("coercion") {
    val z1 = Complex(1, 2)
    expect.all(
      z1 * 2 == Complex(2, 4),
      2 * z1 == Complex(2, 4),
      z1 + 2 == Complex(3, 2)
    )
  }

  test("exponentials") {
    expect(((Pi * im).exp - Complex(-1, 0)).abs < 0.0001)
  }

}
