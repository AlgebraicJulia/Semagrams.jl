package semagrams.graphs

import upickle.default._

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.rendering._
import semagrams.state._
import semagrams.bindings._
import semagrams.partprops._

// import semagrams.util._
// import semagrams.acsets._
// import semagrams.state._
// import semagrams.rendering._
// import semagrams.bindings._

// import semagrams.util.{UndoableVar, UID}
enum GraphOb(val _name: String, val id: UID) extends Ob with Generator
    derives ReadWriter:
  case V extends GraphOb("V", UID("V"))
  case E extends GraphOb("E", UID("E"))

  name = _name
  def generators = this.obGenerators
export GraphOb._

enum GraphHom(
    val _name: String,
    val _id: UID,
    val dom: GraphOb,
    val codom: GraphOb
) extends PartHom
    with Generator:
  case Src extends GraphHom("Src", UID("Src"), E, V)
  case Tgt extends GraphHom("Tgt", UID("Tgt"), E, V)

  name = _name
  val id = _id
  def generators = this.homGenerators
  def path = this.path
export GraphHom._

enum GraphProp[T: ReadWriter] extends PValue[T]:
  case SrcName extends GraphProp[String]
  case TgtName extends GraphProp[String]
export GraphProp._

case object SchGraph extends Schema:

  val id = UID("SchGraph")

  var name = "SchGraph"

  def elts = GraphOb.values.eltMap ++ GraphHom.values.eltMap
  def globalProps = Seq()

object Graph {
  def apply() = ACSet[PropMap](SchGraph)
}

/** Compute the properties (i.e. Start and End) for an edge, using the top-level
  * properties in `acs` and the other sprites in `m`.
  *
  * If `Start`/`End` are already set, it uses those, otherwise it looks up a
  * point on the boundary of the sprite corresponding to the `src`/`tgt` of the
  * edge.
  *
  * Need to update this to look up the sprite for just the first part of
  * src/tgt, and then pass the rest of the path of the part into a method on
  * that sprite.
  */
def spanProps[D: PartData](tag: SpanTag, data: D, m: EntitySeq[D]): PropMap = {
  val SpanTag(ctxt, _, apex, (s, tOpt)) = tag
  val p = data.getProps()
  // val s = p.get(src)
  // val t = p.get(tgt)
  val spos = m
    .findCenter(s)
    .getOrElse(
      p.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = tOpt
    .flatMap(m.findCenter)
    .getOrElse(
      p.get(End).getOrElse(util.Complex(100, 100))
    )
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = util.Complex(0, -bend).exp
  val start = m
    .findBoundary(s, -dir * rot)
    .getOrElse(spos)
  val theend = tOpt
    .flatMap(m.findBoundary(_, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, tOpt) match
    case (p, Some(q)) =>
      PropMap()
        .set(TikzStart, p.keyPart.tikzName)
        .set(TikzEnd, q.keyPart.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, theend)
}

/** Compute the properties (i.e. Start and End) for an edge, using the top-level
  * properties in `acs` and the other sprites in `m`.
  *
  * If `Start`/`End` are already set, it uses those, otherwise it looks up a
  * point on the boundary of the sprite corresponding to the `src`/`tgt` of the
  * edge.
  *
  * Need to update this to look up the sprite for just the first part of
  * src/tgt, and then pass the rest of the path of the part into a method on
  * that sprite.
  */
def homProps[D: PartData](tag: HomTag, data: D, m: EntitySeq[D]): PropMap = {
  val HomTag(ctxt, f, s, tOpt) = tag
  val p = data.getProps()
  // val t = p.get(hom)
  val spos = m
    .findCenter(s)
    .getOrElse(
      p.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = tOpt
    .flatMap(m.findCenter)
    .getOrElse(
      p.get(End).getOrElse(util.Complex(100, 100))
    )
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = util.Complex(0, -bend).exp
  val start = m
    .findBoundary(s, -dir * rot)
    .getOrElse(spos)
  val theend = tOpt
    .flatMap(m.findBoundary(_, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, tOpt) match
    case (p, Some(q)) =>
      PropMap()
        .set(TikzStart, p.keyPart.tikzName)
        .set(TikzEnd, q.keyPart.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, theend)
}

case class GraphDisplay[D: PartData](
    override val modelVar: UndoableVar[ACSet[D]],
    override val bindings: Seq[Binding[ACSet[D]]],
    vertexSources: Seq[ACSetSource[D]],
    edgeSources: Seq[EdgeSource[D]],
    _layout: (ACSet[D], EditorState) => ACSet[D],
    _post: (EntitySeq[D]) => EntitySeq[D]
) extends TabularSemagram[D]:

  val entitySources = vertexSources ++ edgeSources

  def layout(acset: ACSet[D], state: EditorState): ACSet[D] =
    GraphDisplay.defaultLayout(_layout(acset, state), state)

  override def postprocess(entseq: EntitySeq[D]) =
    _post(entseq)

object GraphDisplay:

  def defaultLayout[D: PartData](
      acset: ACSet[D],
      es: EditorState
  ): ACSet[D] =
    acset
      .softSetProp(Highlight, es.hoveredPart.map(_.keyPart), ())
      .softSetProp(Selected, es.selected.map(_.keyPart), ())

  def apply[D: PartData](
      modelVar: UndoableVar[ACSet[D]],
      bindings: Seq[Binding[ACSet[D]]],
      vSource: ObSource[D],
      eSource: EdgeSource[D],
      layout: (ACSet[D], EditorState) => ACSet[D] = (a: ACSet[D], _) => a,
      post: (EntitySeq[D]) => EntitySeq[D] = (es: EntitySeq[D]) => es
  ) =
    new GraphDisplay(
      modelVar,
      bindings,
      Seq(vSource),
      Seq(eSource),
      layout,
      post
    )
