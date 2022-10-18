package semagrams.acsets

import upickle.default._
import cats.data.State
import scala.collection.mutable
import monocle.Lens

import semagrams._

case class ACSet[S: IsSchema](
    schema: S,
    counter: Int,
    parts: Map[Ob, Set[Part]],
    props: Map[Part, PropMap]
) {
  def addPart(ob: Ob, pm: PropMap): (ACSet[S], Part) = {
    val x = Part(counter, ob)
    (
      this.copy(
        parts = parts + (ob -> (parts(ob) + x)),
        counter = counter + 1,
        props = props + (x -> pm)
      ),
      x
    )
  }

  def addPart(ob: Ob): (ACSet[S], Part) = {
    addPart(ob, PropMap())
  }

  def setSubpart(f: Property, x: Part, y: f.Value): ACSet[S] =
    this.copy(
      props = props + (x -> (props(x).set(f, y)))
    )

  def setSubparts(x: Part, pm: PropMap): ACSet[S] =
    this.copy(
      props = props + (x -> (props(x) ++ pm))
    )

  def setSubparts(pms: Iterable[(Part, PropMap)]): ACSet[S] =
    pms.foldLeft(this)({ case (acs, (x, pm)) => acs.setSubparts(x, pm) })

  def remSubpart(f: Property, x: Part): ACSet[S] =
    this.copy(
      props = props + (x -> (props(x) - f))
    )

  def subpart(f: Property, x: Part): f.Value = props(x)(f)

  def trySubpart(f: Property, x: Part): Option[f.Value] = props(x).get(f)

  def incident(f: Property, dom: Ob, y: f.Value): Set[Part] =
    parts(dom).filter(trySubpart(f, _) == Some(y))

  def incident(f: HomWithDom, y: Part): Set[Part] = incident(f, f.dom, y)

  def incident(f: AttrWithDom, y: f.Value): Set[Part] = incident(f, f.dom, y)

  def remPart(x: Part): ACSet[S] = {
    val visited = mutable.HashSet[Part]()
    val next = mutable.Stack[Part](x)
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
      parts = parts.view.mapValues(_ -- visited).toMap,
      props = props.filter((e, _) => !(visited contains e))
    )
  }

  def toSerializable = {
    val sInstance = summon[IsSchema[S]]
    SerializableACSet(
      sInstance.rw.transform(schema, ujson.Value),
      counter,
      parts.map((ob, t) =>
        (ob.toString(), t.map((e: Part) => BarePart(e.id)))
      ),
      props.map((x, m) =>
        (
          BarePart(x.id),
          m.toJson()
        )
      )
    )
  }
}

/** This is to make upickle use a array of arrays instead of a dict for
  * Map[BarePart, X]
  */
case class BarePart(id: Int)

implicit val beRW: ReadWriter[BarePart] =
  readwriter[Int].bimap[BarePart](_.id, BarePart(_))

case class SerializableACSet(
    schema: ujson.Value,
    counter: Int,
    parts: Map[String, Set[BarePart]],
    props: Map[BarePart, Map[String, ujson.Value]]
)

implicit val serializableACSetRW: ReadWriter[SerializableACSet] = macroRW

object ACSet {
  def apply[S: IsSchema](s: S): ACSet[S] = new ACSet[S](
    s,
    0,
    s.obs.map(_ -> Set[Part]()).toMap,
    Map[Part, PropMap]()
  )

  def apply[S]()(implicit iss: IsStaticSchema[S]): ACSet[S] = apply[S](iss.theS)

  def fromSerializable[S](
      sacs: SerializableACSet
  )(implicit sInstance: IsSchema[S]): ACSet[S] = {
    val obsById = mutable.HashMap[Int, Ob]()
    val s = read[S](sacs.schema)(sInstance.rw)
    for ((sob, sparts) <- sacs.parts) {
      val ob = s.obsByString(sob)
      for (x <- sparts) {
        obsById.put(x.id, ob)
      }
    }
    new ACSet[S](
      s,
      sacs.counter,
      sacs.parts.map((sob, sparts) => {
        val ob = s.obsByString(sob)
        val parts = sparts.map((e: BarePart) => Part(e.id, ob))
        (ob, parts)
      }),
      sacs.props.map((x, m) =>
        (
          Part(x.id, obsById(x.id)),
          PropMap.fromJson(genProps ++ s.homsByString ++ s.attrsByString, m)
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
  def addPart(ob: Ob): State[ACSet[S], Part] =
    State(_.addPart(ob))

  def addPart(ob: Ob, props: PropMap): State[ACSet[S], Part] =
    State(_.addPart(ob, props))

  def setSubpart(f: Property, x: Part, y: f.Value): State[ACSet[S], Unit] =
    State.modify(_.setSubpart(f, x, y))

  def remPart(x: Part): State[ACSet[S], Unit] =
    State.modify(_.remPart(x))

  def subpartLens(f: Attr, x: Part) =
    Lens[ACSet[S], f.Value](_.subpart(f, x))(y => s => s.setSubpart(f, x, y))
}

given [S: IsSchema]: ACSetOps[S] = new ACSetOps[S] {}

object ACSetOps {
  def apply[S: IsSchema] = summon[ACSetOps[S]]
}
