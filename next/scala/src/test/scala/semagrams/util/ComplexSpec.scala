package semagrams.util

import utest._
import semagrams.util.Complex
import semagrams.util.Complex._
import scala.math._

object ComplexSpec extends TestSuite {
  def tests = Tests {
    test("arithmetic") {
      val z1 = Complex(1,2)
      val z2 = Complex(2,3)
      assert(-z1 == Complex(-1,-2))
      assert(z1 + z2 == Complex(3,5))
      assert(z1 * z2 == Complex(-4,7))
      assert(z1 - z2 == Complex(-1,-1))
      assert(z1 / Complex(2,0) == Complex(0.5,1))
      assert(z1 / Complex(0,1) == z1 * Complex(0,-1))
    }

    test("coercion") {
      val z1 = Complex(1,2)
      assert(z1 * 2 == Complex(2,4))
      assert(2 * z1 == Complex(2,4))
      assert(z1 + 2 == Complex(3,2))
    }

    test("exponentials") {
      assert(((Pi * im).exp - Complex(-1,0)).abs < 0.0001)
    }
  }
}
