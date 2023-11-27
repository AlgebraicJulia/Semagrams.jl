package semagrams.util

import org.scalajs.dom
import semagrams.GenericProperty
import semagrams.rendering.Sprite


import scala.collection.mutable

private val measurementStyle = """
position: absolute;
visibility: hidden;
height: auto;
width: auto;
white-space: nowrap;
font-family: Alegreya Sans, sans-serif;
"""

private val sizeCache = mutable.Map[(String, Double), Complex]()

/** Returns the dimensions of the bounding box for `text` at `fontSize`
  *
  * This caches computations, so that repeated queries are fast.
  *
  * @todo
  *   we hardcode most of the style for this; we should make this more flexible.
  */
def boxSize(text: String, fontSize: Double, split:Boolean = false): Complex = {
  if (sizeCache contains (text, fontSize)) {
    sizeCache((text, fontSize))
  } else if split
    then 
      val size = boxSize(splitString(text),fontSize)
      sizeCache.addOne((text,fontSize) -> size)
      size
    else {
      val div = dom.document.createElement("div")
      div.setAttribute("style", measurementStyle + s"font-size: ${fontSize}pt;")
      div.innerText = text
      dom.document.body.appendChild(div)
      val size = Complex(div.clientWidth + 1, div.clientHeight + 1)
      dom.document.body.removeChild(div)
      sizeCache.addOne((text, fontSize) -> size)
      size
    }
}

def boxSize(text:Option[String],fontSize:Option[Double]): Complex = boxSize(text,fontSize,false)
def boxSize(text:Option[String],fontSize:Option[Double],split:Boolean): Complex = boxSize(
  text.getOrElse(""),
  fontSize.getOrElse(Sprite.defaultProps(GenericProperty.FontSize)),
  split
)


def boxSize(lines: Seq[String], fontSize: Double): Complex =
  lines.map(boxSize(_,fontSize)).foldLeft(Complex(0,0))(
    (aggr,line) => Complex(aggr.x max line.x,aggr.y + line.y) 
  )


def splitString(s:String): Seq[String] =
  val wds = s.split(" ").toSeq
  val groups = wds.length match
    case 0 => Seq(Seq(""))
    case n if n < 3 => wds.map(Seq(_))
    case n if n == 3 => 
      Seq(wds.take(2),wds.drop(2))
    case n if n % 3 == 0 =>
      Seq(wds.take(n/3),wds.drop(n/3).take(n/3),wds.takeRight(n/3))
    case n if n % 3 == 1 =>
      Seq(wds.take(n/3),wds.drop(n/3).take(n/3 + 1),wds.takeRight(n/3))
    case n if n % 3 == 2 =>
      Seq(wds.take(n/3 + 1),wds.drop(n/3 + 1).take(n/3),wds.takeRight(n/3 + 1))
        
    groups.map(_.mkString(" "))

