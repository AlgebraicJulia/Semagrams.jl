package semagrams

import upickle.default._


import semagrams.util.Complex

import upickle.default.ReadWriter
import semagrams.acsets.abstr.Part

/** An attribute that can be attached to an Entity. */

trait Property {

  /** The type of values of the attribute. */
  type Value

  /** A serializer for those values. */
  val rw: ReadWriter[Value]

  /** Construct a json value from a [[Value]]. */
  def writeValue(v: Any) = {
    rw.transform(v.asInstanceOf[Value], ujson.Value)
  }

  /** Read in a [[Value]] from a json value. */
  def readValue(sv: ujson.Value) = {
    read[Value](sv)(rw)
  }


}

type PartProp = Property { type Value = Part }

/** A subtrait of `Property` that is simpler to implement when there's an
  * implicit ReadWriter for your value type in scope
  */
trait PValue[T: ReadWriter] extends Property {
  type Value = T

  val rw = summon[ReadWriter[T]]
}

enum GenericProperty[T: ReadWriter] extends PValue[T] {
  case Fill extends GenericProperty[String]
  case Stroke extends GenericProperty[String]
  case StrokeDasharray extends GenericProperty[String]
  case InnerSep extends GenericProperty[Double]
  case OuterSep extends GenericProperty[Double]
  case MinimumSize extends GenericProperty[Double]
  case MinimumWidth extends GenericProperty[Double]
  case MinimumHeight extends GenericProperty[Double]
  case FontSize extends GenericProperty[Double]
  case Content extends GenericProperty[String]
  case ImageURL extends GenericProperty[String]
  case Label extends GenericProperty[String]
  case XPos extends GenericProperty[Double]
  case YPos extends GenericProperty[Double]
  case Center extends GenericProperty[Complex]
  case Start extends GenericProperty[Complex]
  case End extends GenericProperty[Complex]
  case Bend extends GenericProperty[Double]
  case RelPos extends GenericProperty[Double]
  case Style extends GenericProperty[String]
  case Interactable extends GenericProperty[Boolean]
  case Hovered extends GenericProperty[Unit]
  case Editing extends GenericProperty[Unit]

  type Value = T

  // val rw = summon[ReadWriter[T]]
}


enum PathProp[T: ReadWriter] extends PValue[T] {
  case StartDir extends PathProp[Complex]
  case EndDir extends PathProp[Complex]
  case WireDir extends PathProp[Complex]
  case LabelAnchor extends PathProp[Double]
  case LabelOffset extends PathProp[Complex]
  case PathLabel extends PathProp[String]
  case TikzStart extends PathProp[String]
  case TikzEnd extends PathProp[String]

  type Value = T
  // val rw = summon[ReadWriter[T]]
}

export PathProp._

export GenericProperty._

val genProps: Map[String, Property] =
  GenericProperty.values.map(p => (p.toString, p)).toMap
