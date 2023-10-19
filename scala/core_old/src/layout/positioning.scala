package semagrams.layout

import semagrams.util.Complex

/** An implementation of BoxPosition is a description of how to place a box in a
  * window.
  */
trait BoxPosition {

  /** Compute the position of the top-left corner of a box according to some
    * formula. This posittion is relative to the coordinate scale where 0+0i is
    * the top left corner of the window, and `windowDims` is the bottom right.
    *
    * @param windowDims
    *   the dimensions of the window that the box is placed in
    *
    * @param boxDims
    *   the dimensions of the box
    */
  def computePosition(windowDims: Complex, boxDims: Complex): Complex
}

/** The data describing the geometry of a box
  *
  * @param pos
  *   the top-left corner of the box
  *
  * @param dims
  *   the dimensions of the box
  */
case class BoxData(pos: Complex, dims: Complex)

/** A reference to a point in coordinates relative to a box
  *
  * @param point
  *   A point in the coordinate scale where 0+0i is the top left corner of the
  *   box, and 1+1i is the bottom right.
  *
  * @param offset
  *   An offset in absolute coordinates from `point`
  */
case class BoxPos(point: Complex, offset: Complex) {

  /** Given a description of a box, compute the actual coordinates of `this`
    *
    * @param bd
    *   the description of the box
    */
  def compute(bd: BoxData) =
    bd.pos + Complex(point.x * bd.dims.x, point.y * bd.dims.y) + offset
}

/** An implementation of [[BoxPosition]] that works by matching up the two
  * points in the window and the box given by `windowPoint` and `boxPoint`
  *
  * @param windowPoint
  *   the point relative to the window to align to
  * @param boxPoint
  *   the point relative to the box to align to
  */
case class AlignPoints(windowPoint: BoxPos, boxPoint: BoxPos)
    extends BoxPosition {

  /** @see
    *   [[BoxPosition.computePosition]]
    */
  def computePosition(windowDims: Complex, boxDims: Complex): Complex =
    windowPoint.compute(BoxData(Complex(0, 0), windowDims)) -
      boxPoint.compute(BoxData(Complex(0, 0), boxDims))
}

/** An object containing several basic instances of `BoxPosition` */
object Align {

  /** Align `p` in the window with `p` in the box */
  def matchingAlign(p: BoxPos) = AlignPoints(p, p)

  /** [[matchingAlign]] with an [[BoxPos.offset]] of 0+0i */
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
