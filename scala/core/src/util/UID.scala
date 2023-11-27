package semagrams.util

import scalajs.js
import upickle.default._


case class UID(stem:String,timestamp:Double,rand:Int) extends Ordered[UID] derives ReadWriter:
  override def toString = Seq(stem,rand).mkString("_")

  def refresh(): UID = UID(stem)

  def compare(that:UID) = this.stem.compare(that.stem) max
    (this.timestamp - that.timestamp).ceil.toInt max
    (this.rand - that.rand)

object UID:
  def apply(stem:String) =
    val timestamp = js.Date.now()
    val rand = (Math.random() * 100000).toInt
    new UID(stem,timestamp,rand)


