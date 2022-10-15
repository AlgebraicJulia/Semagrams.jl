package semagrams.acsets

import upickle.default._

abstract class Subpart {
  type Value
}

abstract class Ob

abstract class Hom extends Subpart {
  type Value = Entity
  val codom: Ob
}

abstract class HomWithDom extends Hom {
  val dom: Ob
}

abstract class Attr extends Subpart {
  type Value

  val rw: ReadWriter[Value]

  def writeValue(v: Any) = {
    rw.transform(v.asInstanceOf[Value], ujson.Value)
  }

  def readValue(sv: ujson.Value) = {
    read[Value](sv)(rw)
  }
}

abstract class AttrWithDom extends Attr {
  val dom: Ob
}

trait IsSchema[S] {
  extension (s: S)
    def obs: Seq[Ob]

    def homs(x: Ob): Seq[Hom]

    def attrs(x: Ob): Seq[Attr]

    def contains(x: Ob): Boolean = obs contains x
    def contains(x: Ob, f: Hom): Boolean = homs(x) contains f
    def contains(x: Ob, f: Attr): Boolean = attrs(x) contains f

  val rw: ReadWriter[S]

  val obRW: ReadWriter[Ob]
  val homRW: ReadWriter[Hom]
  val attrRW: ReadWriter[Attr]
}

case class BasicSchema(
    obs: Map[String, Ob],
    homsByString: Map[String, Hom],
    attrsByString: Map[String, Attr],
    homs: Map[Ob, Seq[Hom]],
    attrs: Map[Ob, Seq[Attr]]
) {
  def extend(
      gens: (Ob | (Ob, Hom) | (Ob, Attr) | HomWithDom | AttrWithDom)*
  ) = {
    val newObs = gens.collect({ case (ob: Ob) => ob })
    val newHoms = gens
      .collect(
        {
          case (hom: HomWithDom)  => (hom.dom, hom)
          case (ob: Ob, hom: Hom) => (ob, hom)
        }
      )
    val newHomsByOb = newHoms.groupMap(_._1)(_._2)
    val newAttrs = gens
      .collect(
        {
          case (attr: AttrWithDom)  => (attr.dom, attr)
          case (ob: Ob, attr: Attr) => (ob, attr)
        }
      )
    val newAttrsByOb = newAttrs.groupMap(_._1)(_._2)
    val homMaps = (homs.toList ++ newObs.map(ob => (ob, Seq[Hom]())))
      .groupMapReduce(_._1)(_._2)(_ ++ _)
    val attrMaps = (attrs.toList ++ newObs.map(ob => (ob, Seq[Attr]())))
      .groupMapReduce(_._1)(_._2)(_ ++ _)
    new BasicSchema(
      obs ++ newObs.map(ob => (ob.toString(), ob)).toMap,
      homsByString ++ newHoms.map((_, f) => (f.toString, f)).toMap,
      attrsByString ++ newAttrs.map((_, f) => (f.toString, f)).toMap,
      homMaps.map((ob, fs) =>
        (ob, fs ++ newHomsByOb.getOrElse(ob, Seq()))
      ),
      attrMaps.map((ob, fs) =>
        (ob, fs ++ newAttrsByOb.getOrElse(ob, Seq()))
      )
    )
  }
}

object BasicSchema {
  def apply(gens: (Ob | (Ob, Hom) | (Ob, Attr) | HomWithDom | AttrWithDom)*) = new BasicSchema(
    Map[String, Ob](),
    Map[String, Hom](),
    Map[String, Attr](),
    Map[Ob, Seq[Hom]](),
    Map[Ob, Seq[Attr]]()
  ).extend(gens*)
}

trait Pointed[S] {
  def value: S
}

implicit def pointedForValueOf[T](implicit vo: ValueOf[T]) : Pointed[T] = new Pointed[T] {
  def value = vo.value
}

trait IsStaticSchema[S : Pointed] extends IsSchema[S] {
  val schema: BasicSchema

  extension (s: S)
    def obs = schema.obs.values.toSeq
    def homs(x: Ob) = schema.homs(x)
    def attrs(x: Ob) = schema.attrs(x)

    override def contains(x: Ob, f: Attr) = f match {
      case (_ : AbstractGenericAttr) => true
      case _ => attrs(x) contains f
    }

  val theS = summon[Pointed[S]].value

  val rw = summon[ReadWriter[Unit]].bimap[S](_ => (), _ => theS)

  val obRW = summon[ReadWriter[String]].bimap[Ob](_.toString, schema.obs(_))
  val homRW = summon[ReadWriter[String]].bimap[Hom](_.toString, schema.homsByString(_))
  val attrRW = summon[ReadWriter[String]].bimap[Attr](_.toString, schema.attrsByString(_))
}

trait StaticSchema {
  val schema: BasicSchema
}

implicit def ssiss[S <: StaticSchema](implicit p: Pointed[S]): IsStaticSchema[S] = new IsStaticSchema {
  val schema = p.value.schema
}
