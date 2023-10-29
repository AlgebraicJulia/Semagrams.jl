package semagrams

// import semagrams.acsets._
import semagrams.util._
import com.raquo.laminar.api.L._
import scala.util.Random
import semagrams.acsets.abstr._

case class BoundingBox(
    pos: Complex,
    dims: Complex
) {
  def sample(): Complex =
    Complex(
      Random.nextDouble() * dims.x + pos.x,
      Random.nextDouble() * dims.y + pos.y
    )
}

type HandlerAttacher = (Entity, SvgElement) => Unit

/** A Sprite contains the information necessary to turn a sub-D into a
  * reactive SVG on the screen.
  *
  * TODO: Sprites should have an "injection" method for statically computing
  * some layout that they want to have available for boundaryPt/bbox queries, or
  * simply adding default properties. This injection is called before present,
  * and the result is saved in the EntityMap. Currently, each sprite has some
  * custom code for being able to have defaults; that should not be custom
  * because then it is inconsistent.
  */
trait Sprite[D:PartData] {

  /** Construct an SvgElement for a subacset.
    *
    * This is the main method to override in an implementation of [[Sprite]].
    *
    * SVG is an absolute medium, so what is returned here is precisely what is
    * shown on the screen; it is not moved or scaled (except possibly by a
    * global transform). Thus, this must use the properties given to fill in all
    * of the necessary parts of the sprite.
    *
    * @param ent
    *   This is the entity that is associated with this Sprite. We pass this
    *   into `present` because `present` attaches handlers to the generated svg
    *   that need to report events that reference `ent`.
    *
    * @param init
    *   This is the initial value of the subacset that specifies the properties
    *   for the sprite. In most cases, we are just interested in the top-level
    *   properties, but sometimes we use the parts inside to, for instance,
    *   display ports. Most of the time, one should get properties from
    *   `updates`, as those will change over time, but there may be some things
    *   that you need in order to construct the svg that will never change, and
    *   those can be taken from `init`.
    *
    * @param updates
    *   This is the same data as `init`, except changing over time. This should
    *   be used to, for instance, set the center of the Sprite, because then the
    *   center will change when the Sprite is dragged.
    *
    * @param attachHandlers
    *   This is used by [[MiddleWare]] to inject event handlers into the sprite.
    *   This should be called on the svg element that the mouse interacts with
    *   (which may be different from the top-level svg element).
    * 
    * @param
    */
  def present(
      p: Part,
      init: D,
      updates: Signal[D],
      eventWriter: Observer[Event]
  ): SvgElement

  def defaultProps: PropMap
  def requiredProps: Seq[Property]

  /** Compute a point on the geometrical boundary of the sprite
    *
    * Note that this does not reference the constructed SVG element, so this can
    * be called with no side effects or global state.
    *
    * This might not make sense, so is optional to implement.
    */
  def boundaryPt(
      data: D,
      dir: Complex,
      subparts: Seq[Part] = Seq()
  ): Option[Complex] = None

  // def boundaryNormal(
  //     data: D,
  //     dir: Complex,
  //     subparts: Seq[Part] = Seq()
  // ): Option[Complex] = None

  /** Compute the geometric center of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def center(
    data: D,
    subparts: Seq[Part] = Seq()
  ): Option[Complex] = None

  /** Compute the bounding box of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def bbox(
    data: D,
    subparts: Seq[Part] = Seq(),
  ): Option[BoundingBox] = None

  /** Convert a diagram element into tikz code */
  def toTikz(p: Part, data: D, visible: Boolean = true): String = ""

  /** An optional layout algorithm to run before rendering an D */
  def layout(bb: BoundingBox, a: D): D = a

  /** Compute the layout for a full window of size `sz` */
  def layoutBg(sz: Complex, a: D): D =
    layout(BoundingBox(sz / 2.0, sz), a)

}


object Sprite:
  def setContent[D:PartData](label:Property)(data:D) =
    data.setProp(Content,
      // data.g
      data.tryProp(label).getOrElse("").toString()
    )

  def innerText[D:PartData](dataSig:Signal[D]): SvgElement = svg.text(
    xy <-- dataSig.map(pm => pm.tryProp(Center).getOrElse(
      throw msgError(s"propmap $pm missing `Center`")
    )),
    children <-- dataSig.map(data =>
      val splits = splitString(data.getProp(Content)).zipWithIndex
      val l = splits.length
      splits.toIndexedSeq.map({ case (t, i) =>
        svg.tspan(
          textToTextNode(t),
          svg.textAnchor := "middle",
          svg.x <-- dataSig.map(p =>
            data.tryProp(Center).getOrElse(Complex(50, 50)).x.toString()
          ),
          svg.y <-- dataSig.map(p =>
            (data.tryProp(Center).getOrElse(Complex(100, 100)).y 
              + data.tryProp(FontSize).getOrElse(12.0) * (i + 1 - l / 2.0)).toString()
          ),
          svg.style := "user-select: none",
        )
      })
    ),
    svg.fontSize <-- dataSig.map(
      _.tryProp(FontSize).getOrElse(12.0).toString
    ),
    svg.pointerEvents := "none",
  )

  // def innerText2(dataSig:Signal[PropMap]): Signal[SvgElement] = 
  //   dataSig.map(pm => svg.text(
  //     xy <-- pm.get(Center).getOrElse(
  //       throw msgError(s"propmap $pm missing `Center`")
  //     ),
  //     children <-- dataSig.map(p =>
  //       val splits = splitString(p(Content)).zipWithIndex
  //       val l = splits.length
  //       splits.toIndexedSeq.map({ case (t, i) =>
  //         svg.tspan(
  //           textToTextNode(t),
  //           svg.textAnchor := "middle",
  //           svg.x <-- dataSig.map(p =>
  //             p.get(Center).getOrElse(Complex(50, 50)).x.toString()
  //           ),
  //           svg.y <-- dataSig.map(p =>
  //             (p.get(Center).getOrElse(Complex(100, 100)).y 
  //               + p.get(FontSize).getOrElse(12.0) * (i + 1 - l / 2.0)).toString()
  //           ),
  //           svg.style := "user-select: none"
  //         )
  //       })
  //     ),
  //     svg.fontSize <-- dataSig.map(
  //       _.get(FontSize).getOrElse(12.0).toString
  //     ),
  //     svg.pointerEvents := "none",
  //   )