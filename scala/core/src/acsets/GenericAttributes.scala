package semagrams.acsets

import semagrams.util._

import upickle.default._

abstract class AbstractGenericAttr extends Attr

sealed abstract class GenericAttr[T: ReadWriter] extends AbstractGenericAttr {
  type Value = T
  val rw = summon[ReadWriter[T]]
}

case object Fill extends GenericAttr[String]
case object Stroke extends GenericAttr[String]
case object InnerSep extends GenericAttr[Double]
case object MinimumSize extends GenericAttr[Double]
case object MinimumWidth extends GenericAttr[Double]
case object MinimumHeight extends GenericAttr[Double]
case object FontSize extends GenericAttr[Double]
case object Content extends GenericAttr[String]
case object Center extends GenericAttr[Complex]
case object Start extends GenericAttr[Complex]
case object End extends GenericAttr[Complex]
case object Bend extends GenericAttr[Double]
case object Style extends GenericAttr[String]

given [T]: ReadWriter[GenericAttr[T]] = macroRW
