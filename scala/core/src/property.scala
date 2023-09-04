package semagrams

import upickle.default._
import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom


import semagrams.util.Complex
import semagrams.widgets._

import upickle.default.ReadWriter
import _root_.widgets._

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

  /** Helper method with special handling for Value = String */ 
  def readStr(s:String): Value = try
    // Errors if Value = String ???
    read[Value](s)(rw)
  catch
    case e =>
      println(e)
      read[Value](write(s))(rw)



  def laminarCell(tSig:Signal[Option[Value]],tObs:Observer[Value]) =
    val editVar = Var(false)
    td(
      cls := "cellTop",
      onDblClick.filter(_ => !editVar.now()).mapTo(true) --> editVar.writer,
      child <-- editVar.signal.map[HtmlElement] {
        case true => tableInput(tSig,
          tObs.contramap(readStr)
        )(rw)
        case false => tableCell(tSig)(rw)
      },
      onKeyPress.filter(_.keyCode == dom.KeyCode.Enter)
        .mapTo(false) --> editVar.writer
    )


}


/** A subtrait of `Property` that is simpler to implement when there's an
  * implicit ReadWriter for your value type in scope
  */
trait PValue[T: ReadWriter] extends Property {
  type Value = T

  val rw = summon[ReadWriter[T]]
}

enum GenericProperty[T: ReadWriter] extends Property {
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

  type Value = T

  val rw = summon[ReadWriter[T]]
}

extension (prop: Property { type Value = String })
  def readStr(s:String) = s    



export GenericProperty._

val genProps: Map[String, Property] =
  GenericProperty.values.map(p => (p.toString, p)).toMap
