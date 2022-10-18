package semagrams.acsets

import semagrams._
import upickle.default._

export semagrams.{Entity, Property}

trait Ob extends EntityType

case class Part(id: Int, ob: Ob) extends Entity {
  def asElt(x: Ob) = if (x == ob) Some(this) else None

  val hash = id
  val entityType = ob
}

trait Hom extends Property {
  type Value = Part
  val codom: Ob

  val rw = summon[ReadWriter[Int]].bimap(_.id, Part(_, codom))
}

trait HomWithDom extends Hom {
  val dom: Ob
}

trait Attr extends Property

trait AttrWithDom extends Attr {
  val dom: Ob
}

trait IsSchema[S] {
  extension (s: S)
    def obs: Seq[Ob]
    def homs(x: Ob): Seq[Hom]
    def attrs(x: Ob): Seq[Attr]

    def obsByString: Map[String, Ob]
    def homsByString: Map[String, Hom]
    def attrsByString: Map[String, Attr]

    def contains(x: Ob): Boolean = obs contains x
    def contains(x: Ob, f: Hom): Boolean = homs(x) contains f
    def contains(x: Ob, f: Attr): Boolean = attrs(x) contains f

  val rw: ReadWriter[S]
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
  ): BasicSchema = {
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
      homMaps.map((ob, fs) => (ob, fs ++ newHomsByOb.getOrElse(ob, Seq()))),
      attrMaps.map((ob, fs) => (ob, fs ++ newAttrsByOb.getOrElse(ob, Seq())))
    )
  }
}

object BasicSchema {
  def apply(gens: (Ob | (Ob, Hom) | (Ob, Attr) | HomWithDom | AttrWithDom)*) =
    new BasicSchema(
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

implicit def pointedForValueOf[T](implicit vo: ValueOf[T]): Pointed[T] =
  new Pointed[T] {
    def value = vo.value
  }

trait IsStaticSchema[S: ValueOf] extends IsSchema[S] {
  val schema: BasicSchema

  extension (s: S)
    def obs = schema.obs.values.toSeq
    def homs(x: Ob) = schema.homs(x)
    def attrs(x: Ob) = schema.attrs(x)

    def obsByString = schema.obs
    def homsByString = schema.homsByString
    def attrsByString = schema.attrsByString

  val theS = summon[ValueOf[S]].value

  val rw = summon[ReadWriter[Unit]].bimap[S](_ => (), _ => theS)
}

trait StaticSchema {
  val schema: BasicSchema

  export schema.extend
}

/** Static Schema Is Static Schema */
implicit def ssiss[S <: StaticSchema](implicit
    p: ValueOf[S]
): IsStaticSchema[S] = new IsStaticSchema {
  val schema = p.value.schema
}
