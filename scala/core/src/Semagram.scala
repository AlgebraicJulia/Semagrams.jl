package semagrams

import com.raquo.laminar.api.L._

import semagrams.acsets._

trait Semagram {
  type Model

  def layout(m: Model): Model

  def render(m: Model, eventWriter: Observer[Event]): Seq[(Entity, ACSet, Sprite)]

  def apply(m: Signal[Model], eventWriter: Observer[Event]): SvgElement = {
    svg.svg()
  }
}
