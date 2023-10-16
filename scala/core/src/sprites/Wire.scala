package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets.abstr._

import semagrams.util.Complex.{im}

import upickle.default.ReadWriter

/** A sprite used for wires. Similar to [[Arrow]], except this one is a spline
  * where the beginning and the end are both horizontal, and it has no
  * arrowhead.
  */
case class Wire[D:PartData]() extends Sprite[D] {

  def exAt(p: Complex, d: Double = 5.0) = {
    import Path.Element._

    Seq(
      MoveTo(p + d * (1 + im)),
      LineTo(p - d * (1 + im)),
      MoveTo(p + d * (1 - im)),
      LineTo(p - d * (1 - im)),
      MoveTo(p)
    )
  }

  def curvedPath(
      z1: Complex,
      z2: Complex,
      dz1: Complex,
      dz2: Complex,
      bend: Double = 1
  ): Seq[Path.Element] = {
    import Path.Element._
    Seq(MoveTo(z1), Cubic(z1 + bend * dz1, z2 + bend * dz2, z2))
  }

  def blockPath(
      z1: Complex,
      z2: Complex,
      dz1: Complex,
      dz2: Complex,
      d: Double,
      bend: Double
  ): Seq[Path.Element] = {
    import Path.Element._

    val crv = Cubic(z1 + bend * dz1, z2 + bend * dz2, z2)

    val ts = (0 to 100).map(_ * .01)
    val p1 = ts.map(t => crv.pos(z1, t) + d * crv.dir(z1, t) * Complex(0, 1))
    val p2 =
      ts.map(t => crv.pos(z1, t) + d * crv.dir(z1, t) * Complex(0, -1)).reverse

    Seq(MoveTo(p1.head))
      ++ p1.tail.map(LineTo(_))
      ++ Seq(LineTo(p2.head))
      ++ p2.tail.map(LineTo(_))
      ++ Seq(ClosePath)
  }

  def present(
      ent: Part,
      init: D,
      updates: L.Signal[D],
      eventWriter: L.Observer[Event]
  ): L.SvgElement = {

    val dataSig = updates.map(Wire.defaults.merge(init).merge(_))



    def ppath(s:Complex,t:Complex,data:D) =
      val Seq(ds,dt): Seq[Complex] = Seq(StartDir,EndDir).map(data.getProp(_))
      val b = data.getProp(Bend)
      curvedPath(s,t,ds,dt,b)

    def textElt(s:Complex,t:Complex,data:D): L.SvgElement =  
      val o = data.getProp(LabelOffset)
      val a = data.getProp(LabelAnchor)
      
      val labelPos = {
        val crv = ppath(s,t,data)(1)
        crv.pos(s, a) + o * crv.dir(s, a)
      }
      val lines = data.getProp(Content).split('\n').zipWithIndex
      val len = lines.length
      def y0(lineNum:Int) = labelPos.y 
            + data.getProp(FontSize) * (lineNum + 1 - len / 2.0)

      def childNode(lineText:String,lineNum:Int) = L.svg.tspan(
        L.textToTextNode(lineText),
        textAnchor := "middle",
        x := labelPos.x.toString(),
        y := y0(lineNum).toString(),
        style := "user-select: none"
      )
            
      L.svg.text(
        xy := labelPos,
        lines.map(childNode),
        fontSize := data.getProp(FontSize).toString(),
        pointerEvents := "none"
      )
    

    def wireElt(s:Complex,t:Complex,data:D): L.SvgElement = 
      path(
        pathElts := ppath(s,t,data),
        stroke := (if data.hasProp(Hovered) 
          then "lightgrey" 
          else data.getProp(Stroke)
        ),
        fill := "none",
        style := "user-select: none",
        pointerEvents := "none"
      )  

    def handleElt(s:Complex,t:Complex,data:D): L.SvgElement = 
      val Seq(ds,dt) = Seq(StartDir,EndDir).map(data.getProp(_))
      val b = data.getProp(Bend)
      path(
        pathElts := blockPath(s, t, ds, dt, 7, b),
        fill := "blue",
        opacity := ".1",
        stroke := "none",
        style := "user-select: none",
        pointerEvents := (if data.tryProp(Interactable) != Some(false)
          then "auto" else "none"
        )
      )

    def elts(s:Complex,t:Complex,data:D) = Seq(
      wireElt(s,t,data),
      handleElt(s,t,data),
      textElt(s,t,data)
    )


    
    
    g(
      L.children <-- dataSig.map(data =>
        (data.tryProp(Start),data.tryProp(End)) match
          case (Some(s0),Some(t0)) =>
            elts(s0,t0,data)
          case _ => Seq()
      )
    )
  }
}
object Wire:
  def defaults[D:PartData] = PartData[D](
    PropMap() + (StartDir,-10)
      + (EndDir,10)
      + (Bend,10)
      + (LabelAnchor,0.5)
      + (LabelOffset,10 * im)
      + (Content,"")
      + (Stroke,"black")
      + (FontSize,16)
  )

