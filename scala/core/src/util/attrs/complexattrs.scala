package semagrams.util

import CustomAttr._

import com.raquo.laminar.api.L
import com.raquo.laminar.api.L.svg._

/** Custom laminar attributes for using complex numbers with svg more easily
  *
  * These can be used both
  *
  *   - with signals, i.e. `xy <-- z`,
  *   - with values, i.e. `xy := z`.
  */
object complexattrs {

  /** A custom svg attribute for setting `x` and `y` from one complex number */
  val xy = new CustomSvgAttr[Complex] {
    def applyAttrs(binder: SvgBinder[Complex]): Unit = {
      binder(x, _.x.toString)
      binder(y, _.y.toString)
    }
  }

  /** A custom svg attribute for setting `cx` and `cy` from one complex number
    */
  val cxy = new CustomSvgAttr[Complex] {
    def applyAttrs(binder: SvgBinder[Complex]): Unit = {
      binder(cx, _.x.toString)
      binder(cy, _.y.toString)
    }
  }

  /** A custom svg attribute for setting `w` and `h` from one complex number */
  val wh = new CustomSvgAttr[Complex] {
    def applyAttrs(binder: SvgBinder[Complex]): Unit = {
      binder(width, _.x.toString)
      binder(height, _.y.toString)
    }
  }

  /** A custom svg attribute for setting `points` from a sequence of complex
    * numbers
    */
  val pointsC = new CustomSvgAttr[Seq[Complex]] {
    def applyAttrs(binder: SvgBinder[Seq[Complex]]): Unit = {
      binder(points, _.map(z => s"${z.x} ${z.y}").mkString(" "))
    }
  }

  /** A custom svg attribute for setting `d` (the svg element for describing a
    * path) from a sequence of [[Path.Element]].
    */
  val pathElts = new CustomSvgAttr[Seq[Path.Element]] {
    def applyAttrs(binder: SvgBinder[Seq[Path.Element]]): Unit = {
      binder(d, _.toSvg)
    }
  }

  /** A custom svg attribute for setting `x1` and `y1` from a complex number */
  val z1 = new CustomSvgAttr[Complex] {
    def applyAttrs(binder: SvgBinder[Complex]): Unit = {
      binder(x1, _.x.toString)
      binder(y1, _.y.toString)
    }
  }

  /** A custom svg attribute for setting `x2` and `y2` from a complex number */
  val z2 = new CustomSvgAttr[Complex] {
    def applyAttrs(binder: SvgBinder[Complex]): Unit = {
      binder(x2, _.x.toString)
      binder(y2, _.y.toString)
    }
  }
}

export complexattrs._

extension [A, B](s: L.Signal[Tuple2[A, B]])
  def splitTuple: Tuple2[L.Signal[A], L.Signal[B]] = (s.map(_._1), s.map(_._2))
