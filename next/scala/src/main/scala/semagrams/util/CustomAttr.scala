package semagrams.util

import com.raquo.laminar.api.L._
import com.raquo.laminar.keys.ReactiveSvgAttr

object CustomAttr {
  trait SvgBinder[Data] {
    def apply[Subpart](attr: ReactiveSvgAttr[Subpart], f: Data => Subpart) : Unit
  }

  case class StaticSvgBinder[Data](el: SvgElement, x: Data) extends SvgBinder[Data] {
    def apply[Subpart](attr: ReactiveSvgAttr[Subpart], f: Data => Subpart) = {
      el.amend(
        attr := f(x)
      )
    }
  }

  case class ReactiveSvgBinder[Data](el: SvgElement, $x: Source[Data]) extends SvgBinder[Data] {
    def apply[Subpart](attr: ReactiveSvgAttr[Subpart], f: Data => Subpart) = {
      val x = $x.toObservable
      el.amend(
        attr <-- x.map(f)
      )
    }
  }

  trait CustomSvgAttr[Data] {
    def applyAttrs(binder: SvgBinder[Data]) : Unit

    def :=(v: Data) = {
      CustomModifier[SvgElement](el =>
        applyAttrs(StaticSvgBinder(el, v))
      )
    }

    def <--($value: Source[Data]) = {
      CustomModifier[SvgElement](el =>
        applyAttrs(ReactiveSvgBinder(el, $value))
      )
    }
  }
}

