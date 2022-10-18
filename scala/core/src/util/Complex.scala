package semagrams.util

import upickle.default._
import scala.language.implicitConversions

import scala.math.{*}

case class Complex(x: Double, y: Double) {
  def +(other: Complex): Complex = Complex(x + other.x, y + other.y)
  def *(other: Complex): Complex =
    Complex(x * other.x - y * other.y, x * other.y + y * other.x)
  def -(other: Complex): Complex = Complex(x - other.x, y - other.y)
  def /(other: Complex): Complex = {
    val d = other.abssq
    val z = this * other.cong
    Complex(z.x / d, z.y / d)
  }

  def unary_- = Complex(-x, -y)

  def exp = {
    val r = pow(E, x)
    Complex(r * cos(y), r * sin(y))
  }

  def cong = Complex(x, -y)

  def abssq = x * x + y * y

  def abs = sqrt(abssq)

  def normalize = this / this.abs

  def toSvg = s"$x $y"
}

implicit def realToComplex(x: Double): Complex = Complex(x, 0)

object Complex {
  val im = Complex(0, 1)
  implicit val rw: ReadWriter[Complex] = macroRW
}
