package semagrams.util

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
