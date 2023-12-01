package semagrams.rendering

import semagrams._
import semagrams.util._
import semagrams.acsets._
import semagrams.partprops._

trait EntitySource[Model, D: PartData] { self =>
  type TagType
  val id: UID
  def makeEntities(m: Model, kvs: EntitySeq[D]): EntitySeq[D]

  def mapData(f: D => D): EntitySource[Model, D] = EntitySource(
    id.refresh(),
    (m: Model, kvs: EntitySeq[D]) =>
      self.makeEntities(m, kvs).map { case k -> (spr, init) =>
        k -> (spr, f(init))
      }
  )

  /** Construct m new [[EntitySource]] which adds the properties in `props` to
    * every acset produced by this [[EntitySource]].
    */
  def withProps(props: PropMap) = mapData(_.setProps(props))

  def withSoftProps(props: PropMap) = mapData(_.softSetProps(props))

  /** Construct m new [[EntitySource]] which uses `f` to make new properties to
    * add to every acset produced by this [[EntitySource]].
    */
  def addPropsBy(f: (PartTag, EntitySeq[D], D) => PropMap) = EntitySource(
    id.refresh(),
    (m: Model, kvs: EntitySeq[D]) =>
      self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
        tag -> (spr, init.setProps(f(tag, kvs, init)))
      }
  )

  def softAddPropsBy(f: (PartTag, EntitySeq[D], D) => PropMap) =
    EntitySource(
      id.refresh(),
      (m: Model, kvs: EntitySeq[D]) =>
        self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
          tag -> (spr, init.softSetProps(f(tag, kvs, init)))
        }
    )

}

object EntitySource:
  def apply[Model, D: PartData](
      id: UID,
      entityConstructor: (Model, EntitySeq[D]) => EntitySeq[D]
  ) = new EntitySource[Model, D] {
    val id = id
    def makeEntities(m: Model, kvs: EntitySeq[D]): EntitySeq[D] =
      entityConstructor(m, kvs)
  }

trait ACSetSource[D: PartData] extends EntitySource[ACSet[D], D]:

  /* Type of schema data for the source
   *   e.g., Ob, Span */
  type Shape

  /* Type of instance data for the source
   *  e.g., Part, (Part,(Part,Part)) */
  type Match

  type TagType <: PartTag

  def sprite: Sprite[D]

  def init: PartialFunction[Shape, D]

  def test = init.isDefinedAt

  def shapes(s: Schema): Seq[Shape]

  def matchData(shape: Shape, acset: ACSet[D]): Seq[(Match, D)]

  def matchData(acset: ACSet[D]): Seq[(Match, D)] =
    shapes(acset.schema).flatMap(matchData(_, acset))

  def matches(shape: Shape, acset: ACSet[D]): Seq[Match] =
    matchData(shape, acset).map(_._1)

  def matches(acset: ACSet[D]): Seq[Match] = matchData(acset).map(_._1)

  def tag(shape: Shape, m: Match): TagType

  def tags(acset: ACSet[D]): Seq[TagType] =
    shapes(acset.schema).flatMap(shape =>
      matches(shape, acset).map(tag(shape, _))
    )

  def baseEntities(acset: ACSet[D], kvs: EntitySeq[D]) = (for
    shape <- shapes(acset.schema)
    if test(shape)
    (m, data) <- matchData(shape, acset)
  yield tag(shape, m) -> (sprite, init(shape).merge(data))).reverse

case class ObSource[D: PartData](
    id: UID,
    sprite: Sprite[D],
    init: PartialFunction[Ob, D]
) extends ACSetSource[D]:
  type Shape = Ob
  type Match = Part

  type TagType = ObTag

  def shapes(s: Schema): Seq[Ob] = s.obSeq

  def matchData(ob: Ob, acset: ACSet[D]): Seq[(Part, D)] =
    acset.getDataSeq(ob)

  def tag(shape: Shape, p: Part) = ObTag(p, id)

  def makeEntities(acset: ACSet[D], kvs: EntitySeq[D]) =
    baseEntities(acset, kvs)

def obdef[D: PartData](kv: (Ob | (Ob, D))): (Ob, D) = kv match
  case kv: (Ob, D) => kv
  case ob: Ob      => ob -> PartData()

object ObSource:
  def apply[D: PartData](id: UID, sprite: Sprite[D], ob: Ob): ObSource[D] =
    new ObSource(id, sprite, Map(ob -> PartData()))
  def apply[D: PartData](
      id: UID,
      sprite: Sprite[D],
      ob: Ob,
      init: D
  ): ObSource[D] =
    new ObSource(id, sprite, Map(ob -> (init)))
  def apply[D: PartData](
      id: UID,
      sprite: Sprite[D],
      kvs: (Ob | (Ob, D))*
  ): ObSource[D] =
    new ObSource(id, sprite, kvs.map(obdef).toMap)

trait EdgeSource[D: PartData] extends ACSetSource[D]:

  def srcTag(m: Match): ObTag
  def tgtTag(m: Match): Option[ObTag]

  def tagSpans(
      acset: ACSet[D]
  ): Seq[(TagType, (PartTag, Option[PartTag]))] =
    shapes(acset.schema).flatMap(shape =>
      matchData(shape, acset).map((m, _) =>
        tag(shape, m) -> (srcTag(m), tgtTag(m))
      )
    )

case class SpanSource[D: PartData](
    contextIds: (UID, (UID, UID)),
    sprite: Sprite[D],
    init: PartialFunction[PartSpan, D]
) extends EdgeSource[D]:
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

  def matchData(shape: Shape, acset: ACSet[D]): Seq[(Match, D)] =
    val Span(f, g) = shape
    for
      (part, data) <- acset.getDataSeq(f.dom)
      if data.hasProps(Seq(f, g)) | data.hasProps(Seq(f, End))
    yield
      val s = data.getProp(f)
      val tOpt = data.tryProp(g)
      val m = part -> (s, tOpt)
      m -> data

  def tag(shape: Shape, m: Match) =
    val (apex, _) = m
    SpanTag(id, shape, apex, srcTag(m) -> tgtTag(m))

  def makeEntities(m: ACSet[D], kvs: EntitySeq[D]): EntitySeq[D] =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr, init.setProps(
        graphs.spanProps(tag, init, kvs)
      ))
    }

object SpanSource:
  // def apply[D: PartData](
  //     vsrc: UID,
  //     sprite: Sprite[D],
  //     span: PartSpan
  // ): SpanSource[D] =
  //   new SpanSource[D](
  //     UID("SpanSource") -> (vsrc, vsrc),
  //     sprite,
  //     Map(span -> PartData())
  //   )
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      span: PartSpan,
      init: D
  ): SpanSource[D] =
    new SpanSource(UID("SpanSource") -> (vsrc, vsrc), sprite, Map(span -> init))
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      kvs: (PartSpan | (PartSpan, D))*
  ): SpanSource[D] =
    val cleankvs = kvs
      .map(_ match
        case span: PartSpan       => span -> PartData[D]()
        case clean: (PartSpan, D) => clean
      )
      .toMap
    new SpanSource(UID("SpanSource") -> (vsrc, vsrc), sprite, cleankvs)

case class HomSource[D: PartData](
    contextIds: (UID, (UID, UID)),
    sprite: Sprite[D],
    init: PartialFunction[PartHom, D]
) extends EdgeSource[D]:
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

  def matchData(f: Shape, acset: ACSet[D]): Seq[(Match, D)] =
    for
      (part, data) <- acset.getDataSeq(f.dom)
      if data.hasProps(Seq(f)) | data.hasProps(Seq(End))
    yield
      val tOpt = data.tryProp(f)
      (part, tOpt) -> data

  def tag(f: Shape, m: Match) =
    val (s, t) = m
    HomTag(id, f, srcTag(m), tgtTag(m))

  def makeEntities(m: ACSet[D], kvs: EntitySeq[D]): EntitySeq[D] =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr, init.setProps(
        graphs.homProps(tag, init, kvs)
      ))
    }

object HomSource:
  def apply[D: PartData](
      vsrc: UID,
      hom: Hom[_],
      sprite: Sprite[D]
  ): HomSource[D] =
    new HomSource[D](
      UID("HomSource") -> (vsrc, vsrc),
      sprite,
      Map(hom -> PartData())
    )
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      hom: Hom[_],
      init: D
  ): HomSource[D] =
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, Map(hom -> init))
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      kvs: (Hom[_] | (Hom[_], D))*
  ): HomSource[D] =
    val cleankvs = kvs
      .map(_ match
        case clean: (Hom[_], D)     => clean
        case hom: Hom[_] @unchecked => hom -> PartData[D]()
      )
      .toMap
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, cleankvs)
