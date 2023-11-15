// package semagrams.sprites

// import com.raquo.laminar.api.L._
// import semagrams._
// import semagrams.util._
// import semagrams.acsets.abstr._

// /** A sprite that wraps an HTML element for use inside the SVG.
//   *
//   * We wrap inside an invisible div that takes up the whole window, so that we
//   * can use CSS logic for the actual positioning.
//   *
//   * @param build
//   *   Constructor for a new HtmlElement
//   *
//   * @param globalSize
//   *   A reference to the size of the overall window
//   */
// case class GenericHTMLSprite[D:PartData](
//     build: () => HtmlElement,
//     globalSize: Signal[Complex]
// ) extends Sprite[D] {

//   def requiredProps = Seq()
//   def defaultProps = PropMap()
//   def present(
//       ent: Part,
//       init: D,
//       updates: Signal[D],
//       eventWriter: Observer[Event]
//   ): SvgElement = {
//     val elt = build()
//     elt.amend(
//       pointerEvents := "auto"
//     )
//     svg.foreignObject(
//       xy := Complex(0, 0),
//       wh <-- globalSize,
//       svg.pointerEvents := "none",
//       div(
//         styleAttr <-- globalSize.map(z =>
//           s"display: flex; width: ${z.x}px; height: ${z.y}px"
//         ),
//         pointerEvents := "none",
//         xmlns := "http://www.w3.org/1999/xhtml",
//         elt
//       )
//     )
//   }

//   // Doesn't really make sense in this context
//   def boundaryPt(data: D, dir: Complex): Complex = Complex(0, 0)

//   def bbox(data: D) = BoundingBox(0, 0)

// }
