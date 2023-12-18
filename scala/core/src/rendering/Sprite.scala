package semagrams.rendering

import semagrams._
import semagrams.state
import semagrams.partprops._
import semagrams.util.{Complex, RGB}

import com.raquo.laminar.api.L._
import scala.util.Random

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

/** A Sprite contains the information necessary to turn a sub-PropMap into a
  * reactive SVG on the screen.
  *
  * TODO: Sprites should have an "injection" method for statically computing
  * some layout that they want to have available for boundaryPt/bbox queries, or
  * simply adding default properties. This injection is called before present,
  * and the result is saved in the EntitySeq. Currently, each sprite has some
  * custom code for being able to have defaults; that should not be custom
  * because then it is inconsistent.
  */
trait Sprite {

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
    * @param eventWriter
    *   This is used propagate local events back to global state.
    */
  def present(
      p: PartTag,
      init: PropMap,
      updates: Signal[PropMap],
      eventWriter: Observer[state.Event]
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
      data: PropMap,
      dir: Complex,
      subparts: Seq[PartTag] = Seq()
  ): Option[Complex] = None

  /** Compute the geometric center of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def center(
      data: PropMap,
      subparts: Seq[PartTag] = Seq()
  ): Option[Complex] = None

  /** Compute the bounding box of the sprite
    *
    * Similar to [[boundaryPt]]
    */
  def bbox(
      data: PropMap,
      subparts: Seq[PartTag] = Seq()
  ): Option[BoundingBox] = None

  /** Convert a diagram element into tikz code */
  def toTikz(p: PartTag, data: PropMap, visible: Boolean = true): String = ""

  /** An optional layout algorithm to run before rendering an PropMap */
  def layout(bb: BoundingBox, a: PropMap): PropMap = a

  /** Compute the layout for a full window of size `sz` */
  def layoutBg(sz: Complex, a: PropMap): PropMap =
    layout(BoundingBox(sz / 2.0, sz), a)

}

object Sprite:

  val defaultProps = PropMap()
    + (Content, "")
    + (ImageURL, "")
    + (FontSize, 14)
    + (Fill, RGB("white"))
    + (Stroke, RGB("black"))
    + (InnerSep, 5)
    + (OuterSep, 5)
    + (MinimumWidth, 40)
    + (MinimumHeight, 40)
    + (Style, "")

  def setContent(label: Property)(data: PropMap) =
    data.set(Content, data.get(label).getOrElse("").toString())

  def innerText(d: PropMap): SvgElement =
    assert(d.contains(Center))
    val data: PropMap = d.softSetProps(Sprite.defaultProps)
    val splits = util.splitString(data(Content)).zipWithIndex
    val l = splits.length

    def xy(idx: Int) =
      val (x0, y0) = data(Center).tuple
      val fs = data(FontSize)
      (x0, y0 + fs * (idx + 1 - l / 2.0))

    val tspans = splits.toIndexedSeq.map { case (text, idx0) =>
      val (x0, y0) = xy(idx0)
      svg.tspan(
        textToTextNode(text),
        svg.textAnchor := "middle",
        svg.x := x0.toString,
        svg.y := y0.toString,
        svg.style := "user-select: none",
        svg.strokeWidth := "0"
      )
    }

    val styles = Seq(
      svg.fontSize := data(FontSize).toString,
      svg.stroke := data
        .get(Stroke)
        .getOrElse(
          defaultProps(Stroke)
        )
        .toString,
      svg.fill := data
        .get(Stroke)
        .getOrElse(
          defaultProps(Stroke)
        )
        .toString,
      svg.pointerEvents := "none"
    )

    svg.text(tspans.map(_.amend(styles)))
