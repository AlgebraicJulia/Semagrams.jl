package semagrams.util

import upickle.default._
import scala.util._
import scala.math.pow

import scalajs.js

/** Helper method with special handling for Value = String */ 
def fromStr[T:ReadWriter](s:String): Option[T] =
  Try(read[T](s)) match
    case Success(t) => Some(t)
    // Fails if T == String. How to check?
    case Failure(_) =>
      Try(read[T](s"\"$s\"")) match
        case Success(t) => Some(t)
        case Failure(_) => None


def toStr[T:ReadWriter](t:T): String = t match
  case t:String => t
  case t => write(t)




case class UID(stem:String,timestamp:Double,rand:Int) derives ReadWriter:
  override def toString = Seq(stem,rand).mkString("_")

object UID:
  def apply(stem:String) =
    val timestamp = js.Date.now()
    val rand = (Math.random() * 100000).toInt
    new UID(stem,timestamp,rand)

