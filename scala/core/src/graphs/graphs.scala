package semagrams.graphs

import upickle.default._

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.rendering._
import semagrams.state._
import semagrams.bindings._
import semagrams.partprops._

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
  def apply() = ACSet(SchGraph)
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
def spanProps(tag: SpanTag, data: PropMap, m: EntitySeq): PropMap = {
  val SpanTag(ctxt, _, apex, (s, tOpt)) = tag
  val spos = m
    .findCenter(s)
    .getOrElse(
      data.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = tOpt
    .flatMap(m.findCenter)
    .getOrElse(
      data.get(End).getOrElse(util.Complex(100, 100))
    )
  val dir = spos - tpos
  val bend = data.get(Bend).getOrElse(0.0)
  val rot = util.Complex(0, -bend).exp
  val start = m
    .findBoundary(s, -dir * rot)
    .getOrElse(spos)
  val theend = tOpt
    .flatMap(m.findBoundary(_, dir * rot.conj))
    .getOrElse(tpos)

  val tikzProps = (s, tOpt) match
    case (data, Some(q)) =>
      PropMap()
        .set(TikzStart, data.keyPart.tikzName)
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
def homProps(tag: HomTag, data: PropMap, m: EntitySeq): PropMap = {
  val HomTag(ctxt, f, s, tOpt) = tag

  val spos = m
    .findCenter(s)
    .getOrElse(
      data.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = tOpt
    .flatMap(m.findCenter)
    .getOrElse(
      data.get(End).getOrElse(util.Complex(100, 100))
    )
  val dir = spos - tpos
  val bend = data.get(Bend).getOrElse(0.0)
  val rot = util.Complex(0, -bend).exp
  val start = m
    .findBoundary(s, -dir * rot)
    .getOrElse(spos)
  val theend = tOpt
    .flatMap(m.findBoundary(_, dir * rot.conj))
    .getOrElse(tpos)

  val tikzProps = (s, tOpt) match
    case (data, Some(q)) =>
      PropMap()
        .set(TikzStart, data.keyPart.tikzName)
        .set(TikzEnd, q.keyPart.tikzName)
    case _ => PropMap()

  tikzProps + (Start, start) + (End, theend)
}

case class GraphDisplay(
    override val modelVar: UndoableVar[ACSet],
    override val bindings: Seq[Binding[ACSet]],
    vertexSources: Seq[ACSetSource],
    edgeSources: Seq[EdgeSource],
    _layout: (ACSet, EditorState) => ACSet,
    _post: (EntitySeq) => EntitySeq
) extends TabularSemagram:

  val entitySources = vertexSources ++ edgeSources

  def layout(acset: ACSet, state: EditorState): ACSet =
    GraphDisplay.defaultLayout(_layout(acset, state), state)

  override def postprocess(entseq: EntitySeq) =
    _post(entseq)

object GraphDisplay:

  def defaultLayout(
      acset: ACSet,
      es: EditorState
  ): ACSet =
    acset
      .softSetProp(Highlight, es.hoveredPart.map(_.keyPart), ())
      .softSetProp(Selected, es.selected.map(_.keyPart), ())

  def apply(
      modelVar: UndoableVar[ACSet],
      bindings: Seq[Binding[ACSet]],
      vSource: ObSource,
      eSource: EdgeSource,
      layout: (ACSet, EditorState) => ACSet = (a: ACSet, _) => a,
      post: (EntitySeq) => EntitySeq = (es: EntitySeq) => es
  ) =
    new GraphDisplay(
      modelVar,
      bindings,
      Seq(vSource),
      Seq(eSource),
      layout,
      post
    )
