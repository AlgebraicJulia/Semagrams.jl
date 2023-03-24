package semagrams.layout

import semagrams.util.Complex

trait BoxPosition {

  /** Given the dimensions of the window, and the dimensions of the box, compute
    * the position of the top-left corner of the box.
    */
  def computePosition(windowDims: Complex, boxDims: Complex): Complex
}

case class BoxData(pos: Complex, dims: Complex)

/** Relative refers to the position inside the box, where 0+0i is the top-left
  * corner, 1+i is the bottom right, and then absolute is an offset in pixels
  * that should be added after the position in the box is calculated.
  */
case class BoxPos(relative: Complex, absolute: Complex) {
  def compute(bd: BoxData) =
    bd.pos + Complex(relative.x * bd.dims.x, relative.y * bd.dims.y) + absolute
}

case class AlignPoints(windowPoint: BoxPos, boxPoint: BoxPos)
    extends BoxPosition {
  def computePosition(windowDims: Complex, boxDims: Complex): Complex =
    windowPoint.compute(BoxData(Complex(0, 0), windowDims)) -
      boxPoint.compute(BoxData(Complex(0, 0), boxDims))
}

object Align {
  def matchingAlign(p: BoxPos) = AlignPoints(p, p)
  def simpleAlign(z: Complex) = matchingAlign(BoxPos(z, Complex(0, 0)))

  val topLeft = simpleAlign(Complex(0, 0))
  val topMiddle = simpleAlign(Complex(0.5, 0))
  val topRight = simpleAlign(Complex(1, 0))
  val middleLeft = simpleAlign(Complex(0, 0.5))
  val middleMiddle = simpleAlign(Complex(0.5, 0.5))
  val middleRight = simpleAlign(Complex(1, 0.5))
  val bottomLeft = simpleAlign(Complex(0, 1))
  val bottomMiddle = simpleAlign(Complex(0.5, 1))
  val bottomRight = simpleAlign(Complex(1, 1))
}
