package semagrams.util

import upickle.default._


case class RGB(red:Int,green:Int,blue:Int):
  override def toString = 
    RGB.colorNames.find((_,rgb) => rgb == this) match
      case Some(name -> color) => name
      case None => s"rgb($red,$green,$blue)"

  def convex(r:Double,x:Int,y:Int) = ((1-r)*x + r*y).toInt
  
  def interp(r:Double,that:RGB) = RGB(
    convex(r,this.red,that.red),
    convex(r,this.green,that.green),
    convex(r,this.blue,that.blue),
  )

  def tuple = (red,green,blue)
  def iter = Seq(red,green,blue)
  
  def lighten(r:Double) = interp(r,RGB(255,255,255))
  def darken(r:Double) = interp(r,RGB(0,0,0))

  /* A measure of brightness between 0 and 1 */
  /* cf. https://en.wikipedia.org/wiki/Relative_luminance */
  def luminance = Seq(0.2126,0.7152,0.0722)
    .zip(iter.map(_/256.0))
    .foldLeft(0.0){ 
      case (aggr,(coeff,color)) => aggr + coeff * color
    }

  def contrast = if luminance > .2 then RGB("black") else RGB("white")




object RGB:

  val rgbPattern = """\s?rgb(\s?[0-9]+\s?,[0-9]+\s?,[0-9]+\s?)\s?""".r

  implicit val  rw:ReadWriter[RGB] = readwriter[String].bimap(
    color => colorNames.find(_._2 == color) match
      case Some(name -> color) => name
      case None => s"rgb(${color.red},${color.green},${color.blue},)"
    ,
    (str:String) => str match
      case str if colorNames.contains(str) => colorNames(str)
      case rgbPattern(r,g,b) => RGB(r.toInt,g.toInt,b.toInt)
      case s => throw msgError(s"bad color rw: $s")
  )

  extension (n:Int)
    def abs = if n < 0 then -n else n

  def apply(r:Int,g:Int,b:Int) = 
    new RGB(r.abs % 256,g.abs % 256,b.abs % 256)

  def apply(str:String) = if colorNames.contains(str)
    then colorNames(str)
    else fromHex(str)

  def fromHex(hexString:String): RGB =
    assert(hexString.length == 6)
    def hex(str:String) = try
      Integer.parseInt(str,16) % 256
    catch
      case e => 255
    
    val r = hex(hexString.slice(0,2))
    val g = hex(hexString.slice(2,4))
    val b = hex(hexString.slice(4,6))
    RGB(r,g,b)

  def colorNames: Map[String,RGB] = Seq(
    "aliceblue" -> "F0F8FF",
    "lightsalmon" -> "FFA07A",
    "antiquewhite" -> "FAEBD7",
    "lightseagreen" -> "20B2AA",
    "aqua" -> "00FFFF",
    "lightskyblue" -> "87CEFA",
    "aquamarine" -> "7FFFD4",
    "lightslategray" -> "778899",
    "azure" -> "F0FFFF",
    "lightsteelblue" -> "B0C4DE",
    "beige" -> "F5F5DC",
    "lightyellow" -> "FFFFE0",
    "bisque" -> "FFE4C4",
    "lime" -> "00FF00",
    "black" -> "000000",
    "limegreen" -> "32CD32",
    "blanchedalmond" -> "FFEBCD",
    "linen" -> "FAF0E6",
    "blue" -> "0000FF",
    "magenta" -> "FF00FF",
    "blueviolet" -> "8A2BE2",
    "maroon" -> "800000",
    "brown" -> "A52A2A",
    "mediumaquamarine" -> "66CDAA",
    "burlywood" -> "DEB887",
    "mediumblue" -> "0000CD",
    "cadetblue" -> "5F9EA0",
    "mediumorchid" -> "BA55D3",
    "chartreuse" -> "7FFF00",
    "mediumpurple" -> "9370DB",
    "chocolate" -> "D2691E",
    "mediumseagreen" -> "3CB371",
    "coral" -> "FF7F50",
    "mediumslateblue" -> "7B68EE",
    "cornflowerblue" -> "6495ED",
    "mediumspringgreen" -> "00FA9A",
    "cornsilk" -> "FFF8DC",
    "mediumturquoise" -> "48D1CC",
    "crimson" -> "DC143C",
    "mediumvioletred" -> "C71585",
    "cyan" -> "00FFFF",
    "midnightblue" -> "191970",
    "darkblue" -> "00008B",
    "mintcream" -> "F5FFFA",
    "darkcyan" -> "008B8B",
    "mistyrose" -> "FFE4E1",
    "darkgoldenrod" -> "B8860B",
    "moccasin" -> "FFE4B5",
    "darkgray" -> "A9A9A9",
    "navajowhite" -> "FFDEAD",
    "darkgreen" -> "006400",
    "navy" -> "000080",
    "darkkhaki" -> "BDB76B",
    "oldlace" -> "FDF5E6",
    "darkmagenta" -> "8B008B",
    "olive" -> "808000",
    "darkolivegreen" -> "556B2F",
    "olivedrab" -> "6B8E23",
    "darkorange" -> "FF8C00",
    "orange" -> "FFA500",
    "darkorchid" -> "9932CC",
    "orangered" -> "FF4500",
    "darkred" -> "8B0000",
    "orchid" -> "DA70D6",
    "darksalmon" -> "E9967A",
    "palegoldenrod" -> "EEE8AA",
    "darkseagreen" -> "8FBC8F",
    "palegreen" -> "98FB98",
    "darkslateblue" -> "483D8B",
    "paleturquoise" -> "AFEEEE",
    "darkslategray" -> "2F4F4F",
    "palevioletred" -> "DB7093",
    "darkturquoise" -> "00CED1",
    "papayawhip" -> "FFEFD5",
    "darkviolet" -> "9400D3",
    "peachpuff" -> "FFDAB9",
    "deeppink" -> "FF1493",
    "peru" -> "CD853F",
    "deepskyblue" -> "00BFFF",
    "pink" -> "FFC0CB",
    "dimgray" -> "696969",
    "plum" -> "DDA0DD",
    "dodgerblue" -> "1E90FF",
    "powderblue" -> "B0E0E6",
    "firebrick" -> "B22222",
    "purple" -> "800080",
    "floralwhite" -> "FFFAF0",
    "red" -> "FF0000",
    "forestgreen" -> "228B22",
    "rosybrown" -> "BC8F8F",
    "fuchsia" -> "FF00FF",
    "royalblue" -> "4169E1",
    "gainsboro" -> "DCDCDC",
    "saddlebrown" -> "8B4513",
    "ghostwhite" -> "F8F8FF",
    "salmon" -> "FA8072",
    "gold" -> "FFD700",
    "sandybrown" -> "FAA460",
    "goldenrod" -> "DAA520",
    "seagreen" -> "2E8B57",
    "gray" -> "808080",
    "seashell" -> "FFF5EE",
    "green" -> "008000",
    "sienna" -> "A0522D",
    "greenyellow" -> "ADFF2F",
    "silver" -> "C0C0C0",
    "honeydew" -> "F0FFF0",
    "skyblue" -> "87CEEB",
    "hotpink" -> "FF69B4",
    "slateblue" -> "6A5ACD",
    "indianred" -> "CD5C5C",
    "slategray" -> "708090",
    "indigo" -> "4B0082",
    "snow" -> "FFFAFA",
    "ivory" -> "FFFFF0",
    "springgreen" -> "00FF7F",
    "khaki" -> "F0E68C",
    "steelblue" -> "4682B4",
    "lavender" -> "E6E6FA",
    "tan" -> "D2B48C",
    "lavenderblush" -> "FFF0F5",
    "teal" -> "008080",
    "lawngreen" -> "7CFC00",
    "thistle" -> "D8BFD8",
    "lemonchiffon" -> "FFFACD",
    "tomato" -> "FF6347",
    "lightblue" -> "ADD8E6",
    "turquoise" -> "40E0D0",
    "lightcoral" -> "F08080",
    "violet" -> "EE82EE",
    "lightcyan" -> "E0FFFF",
    "wheat" -> "F5DEB3",
    "lightgoldenrodyellow" -> "FAFAD2",
    "white" -> "FFFFFF",
    "lightgreen" -> "90EE90",
    "whitesmoke" -> "F5F5F5",
    "lightgrey" -> "D3D3D3",
    "yellow" -> "FFFF00",
    "lightpink" -> "FFB6C1",
    "yellowgreen" -> "9ACD32"
  ).map(
    (name,hexString) => name -> RGB.fromHex(hexString)
  ).toMap

  
