package semagrams.util

import scala.math.pow

extension (t: Double) {

  /** Exponentiation as an infix operator */
  def ^(x: Double) = scala.math.pow(t, x)

  /** Multiplication by a complex implictly converts to complex */
  def *(z: Complex) = Complex(t, 0) * z
}

object Path {

  /** A nicely typed and complex number-using wrapper around path elements in
    * SVG. You can think of these as like "turtle" commands; they specify where
    * to go next, not where to start from.
    */
  enum Element {
    case MoveTo(p: Complex)
    case LineTo(p: Complex)
    case Cubic(cs: Complex, ce: Complex, p: Complex)
    case ClosePath

    /** Construct the SVG string representation of a path element */
    def toSvg = {
      this match {
        case MoveTo(p)        => s"M ${p.toSvg}"
        case LineTo(p)        => s"L ${p.toSvg}"
        case Cubic(cs, ce, p) => s"C ${cs.toSvg} ${ce.toSvg} ${p.toSvg}"
        case ClosePath        => "Z"
      }
    }

    /** Compute the position at "time" `t` along this path element
      *
      * @param z0
      *   the start of the path element
      *
      * @param t
      *   the "time", which should be within [0,1]
      */
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

    /** The direction of the derivative at time `t` of the path.
      *
      * @todo
      *   this should be computed analytically
      */
    def dir(z0: Complex, t: Double): Complex =
      ((pos(z0, t + .01) - pos(z0, t))).normalize
  }
}

extension (elts: Seq[Path.Element]) {

  /** Produce an SVG string representing a whole path. Used in the `d`
    * attribute.
    */
  def toSvg: String = elts.map(_.toSvg).mkString(" ")
}
