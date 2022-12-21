package semagrams.widgets

import com.raquo.laminar.api.L._

enum Margin {
  case Auto
  case Fixed(pixels: Int)

  def toCSS = this match {
    case Auto => "auto"
    case Fixed(pixels) => s"${pixels}px"
  }
}

enum Direction {
  case Top
  case Bottom
  case Left
  case Right
}

type Position = Map[Direction, Margin]

object Position {
  import Direction._
  import Margin._

  val midMid = Map(Top -> Auto, Bottom -> Auto, Left -> Auto, Right -> Auto)

  def botMid(margin: Int) = midMid + (Bottom -> Fixed(margin))
  def topToBotMid(margin: Int) = midMid + (Bottom -> Fixed(margin)) + (Top -> Fixed(margin))
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

def PositionWrapper(p: Position, el: Element) =
  div(
    styleAttr := p.toCSS,
    el
  )
