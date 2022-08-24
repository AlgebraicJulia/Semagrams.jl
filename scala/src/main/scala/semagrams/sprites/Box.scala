package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.util.CustomAttr.CustomSvgAttr
import semagrams.util.CustomAttr.SvgBinder

//FIXME: Move these into util
class XYAttr extends CustomSvgAttr[Complex] {
  def applyAttrs(binder: SvgBinder[Complex]): Unit = {
    binder(x, _.x.toString)
    binder(y, _.y.toString)
  }
}

val xy = new XYAttr()

class WHAttr extends CustomSvgAttr[Complex] {
  def applyAttrs(binder: SvgBinder[Complex]): Unit = {
    binder(width, _.x.toString)
    binder(height, _.y.toString)
  }
}

val wh = new WHAttr()

extension[A,B] (s: L.Signal[Tuple2[A,B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))

case class Box() extends Sprite {
  def geom(data: PropMap): (Complex, Complex) = {
    val center = data(Center())
    val width = data(MinimumWidth())
    val height = data(MinimumHeight())
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
      fill <-- data.map(_(Fill())),
      stroke <-- data.map(_(Stroke()))
    )
  }

  def present(ent: Entity, init: PropMap, updates: L.Signal[PropMap]): RenderedSprite = {
    val root = rect(
      geomUpdater(updates),
      styleUpdater(updates)
    )

    RenderedSprite(root, Map(MainHandle() -> root))
  }

  def boundaryPt(ent: Entity, data: PropMap, dir: Double) = {
    Complex(0,0)
  }
}
