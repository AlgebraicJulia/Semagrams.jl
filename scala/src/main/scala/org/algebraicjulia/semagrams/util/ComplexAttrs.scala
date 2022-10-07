package org.algebraicjulia.semagrams.util

import com.raquo.laminar.api.L.svg._
import CustomAttr._

class XYAttr extends CustomSvgAttr[Complex] {
  def applyAttrs(binder: SvgBinder[Complex]): Unit = {
    binder(x, _.x.toString)
    binder(y, _.y.toString)
  }
}

val xy = new XYAttr()

class CXYAttr extends CustomSvgAttr[Complex] {
  def applyAttrs(binder: SvgBinder[Complex]): Unit = {
    binder(cx, _.x.toString)
    binder(cy, _.y.toString)
  }
}

val cxy = new CXYAttr()

class WHAttr extends CustomSvgAttr[Complex] {
  def applyAttrs(binder: SvgBinder[Complex]): Unit = {
    binder(width, _.x.toString)
    binder(height, _.y.toString)
  }
}

val wh = new WHAttr()

class PointsAttr extends CustomSvgAttr[Seq[Complex]] {
  def applyAttrs(binder: SvgBinder[Seq[Complex]]): Unit = {
    binder(points, _.map(z => s"${z.x} ${z.y}").mkString(" "))
  }
}

val pointsC = new PointsAttr()

class DAttr extends CustomSvgAttr[Seq[Path.Element]] {
  def applyAttrs(binder: SvgBinder[Seq[Path.Element]]): Unit = {
    binder(d, _.toSvg)
  }
}

val pathElts = new DAttr()
