package semagrams.widgets

import com.raquo.laminar.api.L._
import semagrams.util.Complex

/** A typed wrapper around a margin specification */
enum Margin {
  case Auto
  case Fixed(pixels: Int)

  def toCSS = this match {
    case Auto          => "auto"
    case Fixed(pixels) => s"${pixels}px"
  }
}

/** Directions on a computer screen */
enum Direction {
  case Top
  case Bottom
  case Left
  case Right

  def tangent() = this match
    case Top    => Complex(0, 1)
    case Bottom => Complex(0, -1)
    case Left   => Complex(-1, 0)
    case Right  => Complex(1, 0)

}

/** The position of an expandable box, given by a margin in each direction.
  *
  * For instance, a left and right margin of `auto` centers the box.
  */
type Position = Map[Direction, Margin]

/** Some common positions */
object Position {
  import Direction._
  import Margin._

  val midMid = Map(Top -> Auto, Bottom -> Auto, Left -> Auto, Right -> Auto)

  def botMid(margin: Int) = midMid + (Bottom -> Fixed(margin))
  def topToBotMid(margin: Int) =
    midMid + (Bottom -> Fixed(margin)) + (Top -> Fixed(margin))

  def atPos(z: Complex) =
    midMid + (Top -> Fixed(z.y.toInt)) + (Left -> Fixed(z.x.toInt))
}

extension (p: Position) {
  def toCSS = {
    import Margin._
    import Direction._
    s"""
      margin-left: ${p.get(Left).getOrElse(Auto).toCSS};
      margin-right: ${p.get(Right).getOrElse(Auto).toCSS};
      margin-top: ${p.get(Top).getOrElse(Auto).toCSS};
      margin-bottom: ${p.get(Bottom).getOrElse(Auto).toCSS};
    """
  }
}

/** Wrap `el` in a div to position it automatically using CSS */
def PositionWrapper(p: Position, el: Element) =
  div(
    styleAttr := p.toCSS,
    el
  )
