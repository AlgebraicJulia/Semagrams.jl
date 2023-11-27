package semagrams.rendering

import semagrams._
import semagrams.util._
import semagrams.acsets._

trait NewEntitySource[Model, D: PartData] { self =>
  type TagType
  val id: UID
  def makeEntities(m: Model, kvs: EntitySeq[D]): EntitySeq[D]

  def mapData(f: D => D): NewEntitySource[Model, D] = NewEntitySource(
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
  def addPropsBy(f: (EntityTag, EntitySeq[D], D) => PropMap) = NewEntitySource(
    id.refresh(),
    (m: Model, kvs: EntitySeq[D]) =>
      self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
        tag -> (spr, init.setProps(f(tag, kvs, init)))
      }
  )

  def softAddPropsBy(f: (EntityTag, EntitySeq[D], D) => PropMap) =
    NewEntitySource(
      id.refresh(),
      (m: Model, kvs: EntitySeq[D]) =>
        self.makeEntities(m, kvs).map { case tag -> (spr, init) =>
          tag -> (spr, init.softSetProps(f(tag, kvs, init)))
        }
    )

}

object NewEntitySource:
  def apply[Model, D: PartData](
      id: UID,
      entityConstructor: (Model, EntitySeq[D]) => EntitySeq[D]
  ) = new NewEntitySource[Model, D] {
    val id = id
    def makeEntities(m: Model, kvs: EntitySeq[D]): EntitySeq[D] =
      entityConstructor(m, kvs)
  }

trait ACSetSource[D: PartData] extends NewEntitySource[ACSet[D], D]:

  /* Type of schema data for the source
   *   e.g., Ob, Span */
  type Shape

  /* Type of instance data for the source
   *  e.g., Part, (Part,(Part,Part)) */
  type Match

  type TagType <: EntityTag

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

trait EntityTag extends Ordered[EntityTag]:
  type Key
  val contextId: UID
  val key: Key
  def keyPart: Part

  def compare(that: EntityTag) =
    val ctxtOrder = contextId.compare(that.contextId)
    val partOrder = keyPart.id.compare(that.keyPart.id)
    ctxtOrder max (ctxtOrder min partOrder)

case class ObTag(part: Part, contextId: UID) extends EntityTag:
  type Key = Part
  val key = part
  def keyPart = key

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

trait EdgeTag extends EntityTag

case class HomTag(contextId: UID, hom: PartHom, src: Part, tgt: Option[Part])
    extends EdgeTag:
  type Key = (Part, Option[Part])
  val key = src -> tgt
  def keyPart = src

case class SpanTag(
    contextId: UID,
    span: Span,
    apex: Part,
    feet: (Part, Option[Part])
) extends EdgeTag:
  type Key = (Part, (Part, Option[Part]))
  val key = apex -> feet
  def keyPart = apex

trait EdgeSource[D: PartData] extends ACSetSource[D]:

  def srcTag(m: Match, data: D): EntityTag
  def tgtTag(m: Match, data: D): Option[EntityTag]

  def tagSpans(
      acset: ACSet[D]
  ): Seq[(TagType, (EntityTag, Option[EntityTag]))] =
    shapes(acset.schema).flatMap(shape =>
      matchData(shape, acset).map((m, data) =>
        tag(shape, m) -> (srcTag(m, data), tgtTag(m, data))
      )
    )

case class SpanSource[D: PartData](
    contextIds: (UID, (UID, UID)),
    sprite: Sprite[D],
    init: PartialFunction[Span, D]
) extends EdgeSource[D]:
  type Shape = Span
  type Match = (Part, (Part, Option[Part]))

  type TagType = SpanTag

  val (id, (srcId, tgtId)) = contextIds

  def srcTag(m: Match, data: D): EntityTag =
    val (_, (s, _)) = m
    ObTag(s, srcId)

  def tgtTag(m: Match, data: D): Option[EntityTag] =
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
    val (apex, feet) = m
    SpanTag(id, shape, apex, feet)

  def makeEntities(m: ACSet[D], kvs: EntitySeq[D]): EntitySeq[D] =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr, init.setProps(
        graphs.spanProps(tag.span.left, tag.span.right)(
          tag.keyPart,
          init,
          kvs
        )
      ))
    }

object SpanSource:
  def apply[D: PartData](
      vsrc: UID,
      span: Span,
      sprite: Sprite[D]
  ): SpanSource[D] =
    new SpanSource[D](
      UID("SpanSource") -> (vsrc, vsrc),
      sprite,
      Map(span -> PartData())
    )
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      span: Span,
      init: D
  ): SpanSource[D] =
    new SpanSource(UID("SpanSource") -> (vsrc, vsrc), sprite, Map(span -> init))
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      kvs: (Span | (Span, D))*
  ): SpanSource[D] =
    val cleankvs = kvs
      .map(_ match
        case span: Span       => span -> PartData[D]()
        case clean: (Span, D) => clean
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

  def srcTag(m: Match, data: D): EntityTag =
    val (s, _) = m
    ObTag(s, srcId)

  def tgtTag(m: Match, data: D): Option[EntityTag] =
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
    HomTag(id, f, s, t)

  def makeEntities(m: ACSet[D], kvs: EntitySeq[D]): EntitySeq[D] =
    baseEntities(m, kvs).map { case tag -> (spr, init) =>
      tag -> (spr, init.setProps(
        graphs.homProps(tag.hom)(
          tag.keyPart,
          init,
          kvs
        )
      ))
    }

object HomSource:
  def apply[D: PartData](
      vsrc: UID,
      hom: PartHom,
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
      hom: PartHom,
      init: D
  ): HomSource[D] =
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, Map(hom -> init))
  def apply[D: PartData](
      vsrc: UID,
      sprite: Sprite[D],
      kvs: (PartHom | (PartHom, D))*
  ): HomSource[D] =
    val cleankvs = kvs
      .map(_ match
        case clean: (PartHom, D)     => clean
        case hom: PartHom @unchecked => hom -> PartData[D]()
      )
      .toMap
    new HomSource(UID("HomSource") -> (vsrc, vsrc), sprite, cleankvs)
