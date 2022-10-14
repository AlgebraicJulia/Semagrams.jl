package semagrams.acsets

import semagrams.util._

import upickle.default._

case class GenericAttrType[T: ReadWriter]() extends AttrType {
  type Value = T
  val rw: ReadWriter[T] = summon[ReadWriter[T]]
}

sealed abstract class GenericAttribute[T: ReadWriter] extends Attr {
  val codom = GenericAttrType[T]()
}

case object Fill extends GenericAttribute[String]
case object Stroke extends GenericAttribute[String]
case object InnerSep extends GenericAttribute[Double]
case object MinimumSize extends GenericAttribute[Double]
case object MinimumWidth extends GenericAttribute[Double]
case object MinimumHeight extends GenericAttribute[Double]
case object FontSize extends GenericAttribute[Double]
case object Content extends GenericAttribute[String]
case object Center extends GenericAttribute[Complex]
case object Start extends GenericAttribute[Complex]
case object End extends GenericAttribute[Complex]
case object Bend extends GenericAttribute[Double]
case object Style extends GenericAttribute[String]

given [T]: ReadWriter[GenericAttribute[T]] = macroRW
