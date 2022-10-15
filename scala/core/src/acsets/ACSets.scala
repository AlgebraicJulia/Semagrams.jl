package semagrams.acsets

import upickle.default._
import cats.data.State
import scala.collection.mutable
import monocle.Lens

case class Entity(id: Int, ob: Ob)

case class Table(
    parts: Set[Entity],
    subparts: Map[Hom, Map[Entity, Entity]]
) {
  def addPart(ent: Entity) = this.copy(parts = parts + ent)

  def setSubpart(f: Hom, x: Entity, y: Entity): Table = {
    this.copy(
      subparts = subparts + (f -> (subparts(f) + (x -> y)))
    )
  }

  def subpart(f: Hom, x: Entity): Entity =
    subparts(f)(x)

  def trySubpart(f: Hom, x: Entity): Option[f.Value] =
    subparts(f).get(x)

  def remParts(todelete: Set[Entity]): Table =
    this.copy(
      parts = parts -- todelete,
      subparts = subparts.view
        .mapValues(
          _.filter((k, v) => !(todelete contains k) && !(todelete contains v))
        )
        .toMap
    )

  def toSerializable[S](implicit s: IsSchema[S]): SerializableTable = {
    SerializableTable(
      parts.map(_.id),
      subparts.map((f, m) =>
        (s.homRW.transform(f, ujson.Value) -> m.map((k, v) => (k.id, v.id)))
      )
    )
  }
}

case class SerializableTable(
    parts: Set[Int],
    subparts: Map[ujson.Value, Map[Int, Int]]
)

implicit val serializableTableRW: ReadWriter[SerializableTable] = macroRW

object Table {
  def apply[S: IsSchema](s: S, ob: Ob) = new Table(
    Set[Entity](),
    s.homs(ob).map(f => (f, Map[Entity, Entity]())).toMap
  )

  def fromSerializable[S](ob: Ob, t: SerializableTable)(implicit
      s: IsSchema[S]
  ): Table = {
    new Table(
      t.parts.map(Entity(_, ob)),
      t.subparts.map((sf, m) => {
        val f = read[Hom](sf)(s.homRW)
        (f, m.map((k, v) => Entity(k, ob) -> Entity(v, f.codom)))
      })
    )
  }
}

case class AttrMap(map: Map[Attr, Any]) {
  def apply(p: Attr): p.Value = {
    map(p).asInstanceOf[p.Value]
  }

  def get(p: Attr): Option[p.Value] = {
    map.get(p).map(_.asInstanceOf[p.Value])
  }

  def set(k: Attr, v: k.Value): AttrMap = {
    this.copy(map = map + (k -> v.asInstanceOf[Any]))
  }

  def +[T](kv: (GenericAttr[T], T)) =
    this.copy(map = map + (kv._1 -> kv._2.asInstanceOf[Any]))

  def ++(other: AttrMap): AttrMap = {
    this.copy(map = map ++ other.map)
  }
}

object AttrMap {
  def apply() = {
    new AttrMap(Map[Attr, Any]())
  }
}

case class ACSet[S: IsSchema](
    schema: S,
    counter: Int,
    tables: Map[Ob, Table],
    attrs: Map[Entity, AttrMap]
) {

  def parts(x: Ob) = tables(x).parts

  def addPart(x: Ob): (ACSet[S], Entity) = {
    val p = Entity(counter, x)
    (
      this.copy(
        tables = tables + (x -> (tables(x).addPart(p))),
        counter = counter + 1,
        attrs = attrs + (p -> AttrMap())
      ),
      p
    )
  }

  def setSubpart(f: Hom, x: Entity, y: Entity): ACSet[S] =
    this.copy(
      tables = tables + (x.ob -> tables(x.ob).setSubpart(f, x, y))
    )

  def setSubpart(f: Attr, x: Entity, y: f.Value): ACSet[S] =
    this.copy(
      attrs = attrs + (x -> (attrs(x).set(f, y)))
    )

  def subpart(f: Hom, x: Entity): f.Value = tables(x.ob).subpart(f, x)

  def subpart(f: Attr, x: Entity): f.Value = attrs(x)(f)

  def trySubpart(f: Hom, x: Entity): Option[f.Value] =
    tables(x.ob).trySubpart(f, x)

  def trySubpart(f: Attr, x: Entity): Option[f.Value] = attrs(x).get(f)

  def incident(f: Hom, dom: Ob, y: Entity): Set[Entity] =
    tables(dom).parts.filter(trySubpart(f, _) == Some(y))

  def incident(f: Attr, dom: Ob, y: f.Value): Set[Entity] =
    tables(dom).parts.filter(trySubpart(f, _) == Some(y))

  def incident(f: HomWithDom, y: Entity): Set[Entity] = incident(f, f.dom, y)

  def incident(f: AttrWithDom, y: f.Value): Set[Entity] = incident(f, f.dom, y)

  def remPart(x: Entity): ACSet[S] = {
    val visited = mutable.HashSet[Entity]()
    val next = mutable.Stack[Entity](x)
    while (!next.isEmpty) {
      val y = next.pop()
      visited.add(y)
      for (dom <- schema.obs) {
        for (f <- schema.homs(dom).filter(_.codom == y.ob)) {
          next.pushAll(incident(f, dom, y) -- visited)
        }
      }
    }

    this.copy(
      tables = tables.view.mapValues(_.remParts(visited.toSet)).toMap,
      attrs = attrs.filter((e, _) => !(visited contains e))
    )
  }

  def toSerializable = {
    val sInstance = summon[IsSchema[S]]
    SerializableACSet(
      sInstance.rw.transform(schema, ujson.Value),
      counter,
      tables.map((ob, t) =>
        (sInstance.obRW.transform(ob, ujson.Value), t.toSerializable)
      ),
      attrs.map((x, m) =>
        (
          x.id,
          m.map.map((f, v) =>
            (sInstance.attrRW.transform(f, ujson.Value), f.writeValue(v))
          )
        )
      )
    )
  }
}

case class SerializableACSet(
    schema: ujson.Value,
    counter: Int,
    tables: Map[ujson.Value, SerializableTable],
    attrs: Map[Int, Map[ujson.Value, ujson.Value]]
)

implicit val serializableACSetRW: ReadWriter[SerializableACSet] = macroRW

object ACSet {
  def apply[S: IsSchema](s: S): ACSet[S] = new ACSet[S](
    s,
    0,
    s.obs.map(ob => (ob -> Table(s, ob))).toMap,
    Map[Entity, AttrMap]()
  )

  def apply[S]()(implicit iss: IsStaticSchema[S]): ACSet[S] = apply[S](iss.theS)

  def fromSerializable[S](
      sacs: SerializableACSet
  )(implicit s: IsSchema[S]): ACSet[S] = {
    val obsById = mutable.HashMap[Int, Ob]()
    for ((sob, t) <- sacs.tables) {
      val ob = read[Ob](sob)(s.obRW)
      for (x <- t.parts) {
        obsById.put(x, ob)
      }
    }
    new ACSet[S](
      read[S](sacs.schema)(s.rw),
      sacs.counter,
      sacs.tables.map((sob, st) => {
        val ob = read[Ob](sob)(s.obRW)
        val t = Table.fromSerializable(ob, st)
        (ob, t)
      }),
      sacs.attrs.map((x, m) =>
        (
          Entity(x, obsById(x)),
          new AttrMap(m.map((sf, sv) => {
            val f = read[Attr](sf)(s.attrRW)
            (f, f.readValue(sv))
          }))
        )
      )
    )
  }

  def rw[S: IsSchema]: ReadWriter[ACSet[S]] =
    serializableACSetRW.bimap[ACSet[S]](
      _.toSerializable,
      fromSerializable[S]
    )
}

trait ACSetOps[S: IsSchema] {
  def addPart(ob: Ob): State[ACSet[S], Entity] =
    State(_.addPart(ob))

  def setSubpart(f: Hom, x: Entity, y: Entity): State[ACSet[S], Unit] =
    State.modify(_.setSubpart(f, x, y))

  def setSubpart(f: Attr, x: Entity, y: f.Value): State[ACSet[S], Unit] =
    State.modify(_.setSubpart(f, x, y))

  def remPart(x: Entity): State[ACSet[S], Unit] =
    State.modify(_.remPart(x))

  def subpartLens(f: Attr, x: Entity) =
    Lens[ACSet[S], f.Value](_.subpart(f, x))(y => s => s.setSubpart(f, x, y))
}
