package semagrams.util

import upickle.default._
import scala.language.implicitConversions

import scala.math.{*}
import java.util.InputMismatchException

/** A class representing a complex number
  *
  * We use complex numbers for two-dimensional geometry because they can
  * represent either:
  *
  *   - A point
  *   - A vector
  *   - A translation
  *   - A rotation/scaling
  *
  * Instead of having to remember APIs for all of these, you can just use
  * familiar arithmetic operations.
  *
  * The operations here are the standard mathematical ones for the complex
  * numbers.
  */
case class Complex(x: Double, y: Double) {
  def +(other: Complex): Complex = Complex(x + other.x, y + other.y)
  def *(other: Complex): Complex =
    Complex(x * other.x - y * other.y, x * other.y + y * other.x)
  def -(other: Complex): Complex = Complex(x - other.x, y - other.y)
  def /(other: Complex): Complex = {
    val d = other.abssq
    val z = this * other.conj
    Complex(z.x / d, z.y / d)
  }

  def dot(other: Complex) = x * other.x + y * other.y

  def scaleTo(that: Complex) = Complex(this.x * that.x, this.y * that.y)

  def scaleFrom(that: Complex) = Complex(this.x / that.x, this.y / that.y)

  def unary_- = Complex(-x, -y)

  def exp = {
    val r = pow(E, x)
    Complex(r * cos(y), r * sin(y))
  }

  def log = Math.log(abs) + Math.atan2(x, y)

  def conj = Complex(x, -y)

  /** absolute value squared */
  def abssq = x * x + y * y

  /** absolute value */
  def abs = sqrt(abssq)

  def normalize = this.abs match
    case 0.0 => Complex(1, 0)
    case r   => this / r

  /** convert to an SVG string */
  def toSvg = s"$x $y"

  def tuple = (x, y)
  def iter = Seq(x, y)
}

object Complex {
  val im = Complex(0, 1)
  val one = Complex(1, 0)
  implicit val rw: ReadWriter[Complex] = macroRW

  def apply(x: Double, y: Double) = if x.isNaN || y.isNaN then
    throw new InputMismatchException
  else new Complex(x, y)

  implicit def realToComplex(x: Double): Complex = Complex(x, 0)
  implicit def intToComplex(n: Int): Complex = Complex(n, 0)
}
