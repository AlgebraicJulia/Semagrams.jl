package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.text._

//FIXME: Move these into util

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))

case class Box() extends Sprite {
  def geom(data: PropMap): (Complex, Complex) = {
    val textBox = boxSize(data(Content), data(FontSize))
    val center = data(Center)
    val innerSep = data(InnerSep)
    val width = data(MinimumWidth).max(textBox.x + innerSep)
    val height = data(MinimumHeight).max(textBox.y + innerSep)
    val dims = Complex(width, height)
    val pos = center - dims / 2
    (pos, dims)
  }

  def geomUpdater(updates: L.Signal[PropMap]) = {
    val (pos, dims) = updates.map(geom).splitTuple
    List(xy <-- pos, wh <-- dims)
  }

  def styleUpdater(data: L.Signal[PropMap]) = {
    List(
      fill <-- data.map(_(Fill)),
      stroke <-- data.map(_(Stroke)),
      style <-- data.map(_.get(Style).getOrElse(""))
    )
  }

  def present(
      ent: Entity,
      init: PropMap,
      updates: L.Signal[PropMap]
  ): RenderedSprite = {
    val box = rect(
      geomUpdater(updates),
      styleUpdater(updates)
    )
    val text = L.svg.text(
      xy <-- updates.map(_(Center)),
      L.svg.tspan(
        L.child <-- updates.map(p => L.textToNode(p(Content))),
        textAnchor := "middle",
        dominantBaseline := "central",
        style := "user-select: none"
      ),
      fontSize <-- updates.map(_(FontSize).toString)
    )

    val root = g(
      box,
      text
    )

    RenderedSprite(root, Map(MainHandle -> root))
  }

  def boundaryPt(data: PropMap, dir: Complex) = {
    // Normalize to first quadrant
    val (_, dims) = geom(data)
    val q1dir = Complex(dir.x.abs, dir.y.abs)
    val q1pt = if (q1dir.x == 0) {
      Complex(0, dims.y / 2)
    } else if (q1dir.y == 0) {
      Complex(dims.x / 2, 0)
    } else if (q1dir.x * dims.y > q1dir.y * dims.x) {
      Complex(
        dims.x / 2,
        (q1dir.y * dims.x) / (q1dir.x * 2)
      )
    } else {
      Complex(
        (q1dir.x * dims.y) / (q1dir.y * 2),
        dims.y / 2
      )
    }
    Complex(q1pt.x * dir.x.sign, q1pt.y * dir.y.sign) + data(Center)
  }

  def boundaryNormal(data: PropMap,dir: Complex): Complex = {
    val (pos,dims) = geom(data)
    (dims.x,dims.y,dir.x,dir.y) match
      // Vertical
      case (_,_,0,y) => if y >= 0 then Complex(0,1) else Complex(0,-1)
      // Zero width
      case (0,y,x,_) => if x >= 0 then Complex(1,0) else Complex(-1,0)
      // General position
      case (x1,y1,x2,y2) => 
        val slope1 = (y1/x1).abs
        val slope2 = (y2/x2).abs
        if slope2 >= slope1 
          then 
            if y2 >= 0 
            then Complex(0,1)
            else Complex(0,-1)
          else 
            if x2 >= 0
            then Complex(1,0)
            else Complex(-1,0)
  }


}