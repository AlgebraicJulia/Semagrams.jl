package semagrams.util

import scala.math.pow

extension (t: Double) {
  def ^(x: Double) = scala.math.pow(t, x)
  def *(z: Complex) = Complex(t, 0) * z
}

object Path {
  enum Element {
    case MoveTo(p: Complex)
    case LineTo(p: Complex)
    case Cubic(cs: Complex, ce: Complex, p: Complex)
    case ClosePath

    def toSvg = {
      this match {
        case MoveTo(p)        => s"M ${p.toSvg}"
        case LineTo(p)        => s"L ${p.toSvg}"
        case Cubic(cs, ce, p) => s"C ${cs.toSvg} ${ce.toSvg} ${p.toSvg}"
        case ClosePath        => "Z"
      }
    }

    def pos(z0: Complex, t: Double): Complex = this match {
      case MoveTo(p) => (1 - t) * z0 + t * p
      case LineTo(p) => (1 - t) * z0 + t * p
      case Cubic(cs, ce, p) => (
        ((1 - t) ^ 3) * z0
          + 3 * ((1 - t) ^ 2) * t * cs
          + 3 * (1 - t) * (t ^ 2) * ce
          + (t ^ 3) * p
      )
      case ClosePath => z0
    }

    def dir(z0: Complex, t: Double): Complex =
      ((pos(z0, t + .01) - pos(z0, t))).normalize
  }
}

extension (elts: Seq[Path.Element]) {
  def toSvg: String = elts.map(_.toSvg).mkString(" ")
}
