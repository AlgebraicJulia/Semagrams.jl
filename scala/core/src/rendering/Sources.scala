package semagrams.rendering

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.partprops._

trait EntitySource[Model] { self =>
  type TagType
  val id: UID
  def makeEntities(m: Model, kvs: EntitySeq): EntitySeq

  def mapData(f: PropMap => PropMap): EntitySource[Model] =
    EntitySource(
      id.refresh(),
      (m: Model, kvs: EntitySeq) =>
        self.makeEntities(m, kvs).map { case k -> (spr, init) =>
          k -> (spr, f(init))
        }
    )

  /** Construct m new [[EntitySource]] which adds the properties in `props` to
    * every acset produced by this [[EntitySource]].
    */
  def withProps(props: PropMap) = mapData(_ ++ props)

  def withSoftProps(props: PropMap) = mapData(_.softSetProps(props))

  /** Construct m new [[EntitySource]] which uses `f` to make new properties to
    * add to every acset produced by this [[EntitySource]].
    */
  def addPropsBy(f: (PartTag, EntitySeq, PropMap) => PropMap) = EntitySource(
    id.refresh(),
    (m: Model, kvs: EntitySeq) =>
      self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
        tag -> (spr, init ++ f(tag, kvs, init))
      }
  )

  def softAddPropsBy(f: (PartTag, EntitySeq, PropMap) => PropMap) =
    EntitySource(
      id.refresh(),
      (m: Model, kvs: EntitySeq) =>
        self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
          tag -> (spr, init.softSetProps(f(tag, kvs, init)))
        }
    )

}

object EntitySource:
  def apply[Model, PropMap](
      id: UID,
      entityConstructor: (Model, EntitySeq) => EntitySeq
  ) = new EntitySource[Model] {
    val id = id
    def makeEntities(m: Model, kvs: EntitySeq): EntitySeq =
      entityConstructor(m, kvs)
  }

trait ACSetSource extends EntitySource[ACSet]:

  /* Type of schema data for the source
   *   e.g., Ob, Span */
  type Shape

  /* Type of instance data for the source
   *  e.g., Part, (Part,(Part,Part)) */
  type Match

  type TagType <: PartTag

  def sprite: Sprite

  def init: PartialFunction[Shape, PropMap]

  def test = init.isDefinedAt

  def shapes(s: Schema): Seq[Shape]

  def matchData(shape: Shape, acset: ACSet): Seq[(Match, PropMap)]

  def matchData(acset: ACSet): Seq[(Match, PropMap)] =
    shapes(acset.schema).flatMap(matchData(_, acset))

  def matches(shape: Shape, acset: ACSet): Seq[Match] =
    matchData(shape, acset).map(_._1)

  def matches(acset: ACSet): Seq[Match] = matchData(acset).map(_._1)

  def tag(shape: Shape, m: Match): TagType

  def tags(acset: ACSet): Seq[TagType] =
    shapes(acset.schema).flatMap(shape =>
      matches(shape, acset).map(tag(shape, _))
    )

  def baseEntities(acset: ACSet, kvs: EntitySeq) = (for
    shape <- shapes(acset.schema)
    if test(shape)
    (m, data) <- matchData(shape, acset)
  yield tag(shape, m) -> (sprite, init(shape) ++ data)).reverse

case class ObSource(
    id: UID,
    sprite: Sprite,
    init: PartialFunction[Ob, PropMap]
) extends ACSetSource:
  type Shape = Ob
  type Match = Part

  type TagType = ObTag

  def shapes(s: Schema): Seq[Ob] = s.obSeq

  def matchData(ob: Ob, acset: ACSet): Seq[(Part, PropMap)] =
    acset.getPropSeq(ob)

  def tag(shape: Shape, p: Part) = ObTag(p, id)

  def makeEntities(acset: ACSet, kvs: EntitySeq) =
    baseEntities(acset, kvs)

def obdef(kv: (Ob | (Ob, PropMap))): (Ob, PropMap) = kv match
  case kv: (Ob, PropMap) => kv
  case ob: Ob            => ob -> PropMap()

object ObSource:
  def apply(id: UID, sprite: Sprite, ob: Ob): ObSource =
    new ObSource(id, sprite, Map(ob -> PropMap()))
  def apply(
      id: UID,
      sprite: Sprite,
      ob: Ob,
      init: PropMap
  ): ObSource =
    new ObSource(id, sprite, Map(ob -> (init)))
  def apply(
      id: UID,
      sprite: Sprite,
      kvs: (Ob | (Ob, PropMap))*
  ): ObSource =
    new ObSource(id, sprite, kvs.map(obdef).toMap)

trait EdgeSource extends ACSetSource:

  def srcTag(m: Match): ObTag
  def tgtTag(m: Match): Option[ObTag]

  def tagSpans(
      acset: ACSet
  ): Seq[(TagType, (PartTag, Option[PartTag]))] =
    shapes(acset.schema).flatMap(shape =>
      matchData(shape, acset).map((m, _) =>
        tag(shape, m) -> (srcTag(m), tgtTag(m))
      )
    )

case class SpanSource(
    contextIds: (UID, (UID, UID)),
    sprite: Sprite,
    init: PartialFunction[PartSpan, PropMap]
) extends EdgeSource:
  type Shape = PartSpan
  type Match = (Part, (Part, Option[Part]))

  type TagType = SpanTag

  val (id, (srcId, tgtId)) = contextIds

  def srcTag(m: Match): ObTag =
    val (_, (s, _)) = m
    ObTag(s, srcId)

  def tgtTag(m: Match): Option[ObTag] =
    val (_, (_, t)) = m
    t.map(ObTag(_, tgtId))

  def shapes(s: Schema): Seq[Shape] =
    for
      f <- s.homSeq.collect { case f: PartHom => f }
      g <- s.homSeq.collect { case f: PartHom => f }
      if f.dom == g.dom
      span = Span(f, g)
      if test(span)
    yield span

  def matchData(shape: Shape, acset: ACSet): Seq[(Match, PropMap)] =
    val Span(f, g) = shape
    for
      (part, data) <- acset.getPropSeq(f.dom)
      if data.contains(f, g) | data.contains(f, End)
    yield
      val s = data(f)
      val tOpt = data.get(g)
      val m = part -> (s, tOpt)
      m -> data

  def tag(shape: Shape, m: Match) =
    val (apex, _) = m
    SpanTag(id, shape, apex, srcTag(m) -> tgtTag(m))

  def makeEntities(m: ACSet, kvs: EntitySeq): EntitySeq =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr,
      init ++ graphs.spanProps(tag, init, kvs))
    }

object SpanSource:
  // def apply(
  //     vsrc: UID,
  //     sprite: Sprite,
  //     span: PartSpan
  // ): SpanSource =
  //   new SpanSource(
  //     UID("SpanSource") -> (vsrc, vsrc),
  //     sprite,
  //     Map(span -> PropMap())
  //   )
  def apply(
      vsrc: UID,
      sprite: Sprite,
      span: PartSpan,
      init: PropMap
  ): SpanSource =
    new SpanSource(UID("SpanSource") -> (vsrc, vsrc), sprite, Map(span -> init))
  def apply(
      vsrc: UID,
      sprite: Sprite,
      kvs: (PartSpan | (PartSpan, PropMap))*
  ): SpanSource =
    val cleankvs = kvs
      .map(_ match
        case span: PartSpan             => span -> PropMap()
        case clean: (PartSpan, PropMap) => clean
      )
      .toMap
    new SpanSource(UID("SpanSource") -> (vsrc, vsrc), sprite, cleankvs)

case class HomSource(
    contextIds: (UID, (UID, UID)),
    sprite: Sprite,
    init: PartialFunction[PartHom, PropMap]
) extends EdgeSource:
  type Shape = PartHom
  type Match = (Part, Option[Part])

  type TagType = HomTag

  val (id, (srcId, tgtId)) = contextIds

  def srcTag(m: Match): ObTag =
    val (s, _) = m
    ObTag(s, srcId)

  def tgtTag(m: Match): Option[ObTag] =
    val (_, t) = m
    t.map(ObTag(_, tgtId))

  def shapes(s: Schema): Seq[Shape] =
    s.homSeq
      .collect { case f: PartHom => f }
      .filter(test)

  def matchData(f: Shape, acset: ACSet): Seq[(Match, PropMap)] =
    for
      (part, data) <- acset.getPropSeq(f.dom)
      if data.contains(f) | data.contains(End)
    yield
      val tOpt = data.get(f)
      (part, tOpt) -> data

  def tag(f: Shape, m: Match) =
    val (s, t) = m
    HomTag(id, f, srcTag(m), tgtTag(m))

  def makeEntities(m: ACSet, kvs: EntitySeq): EntitySeq =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr,
      init ++ graphs.homProps(tag, init, kvs))
    }

object HomSource:
  def apply(
      vsrc: UID,
      hom: Hom[_],
      sprite: Sprite
  ): HomSource =
    new HomSource(
      UID("HomSource") -> (vsrc, vsrc),
      sprite,
      Map(hom -> PropMap())
    )
  def apply(
      vsrc: UID,
      sprite: Sprite,
      hom: Hom[_],
      init: PropMap
  ): HomSource =
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, Map(hom -> init))
  def apply(
      vsrc: UID,
      sprite: Sprite,
      kvs: (Hom[_] | (Hom[_], PropMap))*
  ): HomSource =
    val cleankvs = kvs
      .map(_ match
        case clean: (Hom[_], PropMap) => clean
        case hom: Hom[_] @unchecked   => hom -> PropMap()
      )
      .toMap
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, cleankvs)
