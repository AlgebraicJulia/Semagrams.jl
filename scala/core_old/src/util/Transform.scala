package semagrams.util

/** Represents an affine transformation of the 2d plane
  *
  * @param z
  *   the scaling/twisting factor
  *
  * @param w
  *   the translation factor
  *
  * Note: scaling is applied before translation.
  */
case class Transform(z: Complex, w: Complex) {

  /** Returns a string representing this transform in SVG style */
  def toSvg: String =
    s"matrix(${z.x} ${z.y} ${-z.y} ${z.x} ${w.x} ${w.y})"

  /** Returns a new transform that is zoomed in by `factor`.
    *
    * The old and new transforms send the origin to the same point.
    */
  def zoomBy(factor: Double): Transform = this.copy(z = z * factor)

  /** Returns a new transform that is zoomed in on `pos` by `factor`
    *
    * The old and new transforms send `pos` to the same point.
    */
  def zoomAtPos(factor: Double, pos: Complex): Transform = {
    val zp = z * factor
    Transform(zp, w + (z.x - zp.x) * pos)
  }

  /** Apply the transform to a point */
  def apply(p: Complex): Complex = p * z + w

  /** Apply inverse of the transform */
  def inverse(p: Complex): Complex = (p - w) / z

  /** Returns a new transform that is shifted by `by` */
  def shift(by: Complex): Transform = this.copy(w = w + by)
}

object Transform {

  /** The identity affine transformation */
  val identity = Transform(1, 0)
}
