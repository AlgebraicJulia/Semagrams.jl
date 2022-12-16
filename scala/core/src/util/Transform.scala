package semagrams.util

case class Transform(z: Complex, w: Complex) {
  def toSvg =
    s"matrix(${z.x} ${z.y} ${-z.y} ${z.x} ${w.x} ${w.y})"

  def zoomBy(factor: Double) = this.copy(z = z * factor)

  def zoomAtPos(factor: Double, pos: Complex) = {
    val zp = z * factor
    Transform(zp, w + (z.x - zp.x) * pos)
  }

  def apply(p: Complex) = p * z + w

  def inverse(p: Complex) = (p - w) / z

  def shift(by: Complex) = this.copy(w = w + by)
}

object Transform {
  val identity = Transform(1, 0)
}
