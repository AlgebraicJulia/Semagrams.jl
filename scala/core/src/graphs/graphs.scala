package semagrams.graphs

import upickle.default._
import com.raquo.laminar.api.L._

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.state._
import semagrams.rendering._
import semagrams.bindings._

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
) extends Hom[GraphOb, GraphOb]
    with Generator:
  case Src extends GraphHom("Src", UID("Src"), E, V)
  case Tgt extends GraphHom("Tgt", UID("Tgt"), E, V)

  type Value = Part
  val rw = Part.rw
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
def spanProps[D: PartData](
    src: PartProp,
    tgt: PartProp
)(_e: Part, data: D, m: EntitySeq[D]): PropMap = {
  val p = data.getProps()
  val s = p.get(src)
  val t = p.get(tgt)
  val spos = s
    .flatMap(m.findCenter)
    .getOrElse(
      p.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = t
    .flatMap(m.findCenter)
    .getOrElse(
      p.get(End).getOrElse(util.Complex(100, 100))
    )
  val dir = spos - tpos
  val bend = p.get(Bend).getOrElse(0.0)
  val rot = util.Complex(0, -bend).exp
  val start = s
    .flatMap(m.findBoundary(_, -dir * rot))
    .getOrElse(spos)
  val theend = t
    .flatMap(m.findBoundary(_, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, t) match
    case (Some(p), Some(q)) =>
      PropMap().set(TikzStart, p.tikzName).set(TikzEnd, q.tikzName)
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
def homProps[D: PartData](
    hom: PartProp
)(s: Part, data: D, m: EntitySeq[D]): PropMap = {
  val p = data.getProps()
  val t = p.get(hom)
  val spos = m
    .findCenter(s)
    .getOrElse(
      p.get(Start).getOrElse(util.Complex(100, 100))
    )
  val tpos = t
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
  val theend = t
    .flatMap(m.findBoundary(_, dir * rot.cong))
    .getOrElse(tpos)

  val tikzProps = (s, t) match
    case (p, Some(q)) =>
      PropMap().set(TikzStart, p.tikzName).set(TikzEnd, q.tikzName)
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

  def defaultLayout[D: PartData](acset: ACSet[D], es: EditorState): ACSet[D] =
    acset
      .softSetProp(Highlight, es.hoveredPart, ())
      .softSetProp(Selected, es.selected, ())

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
