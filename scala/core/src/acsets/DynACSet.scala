package semagrams.acsets

import upickle.default._
import scala.collection.mutable

case class DynOb(name: String) extends Ob {
  override def toString() = name
}

given ReadWriter[DynOb] = macroRW

case class DynHom(name: String, dom: DynOb, codom: DynOb) extends HomWithDom {
  override def toString() = name
}

given ReadWriter[DynHom] = readwriter[Map[String, String]].bimap[DynHom](
  f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
  sf => DynHom(sf("name"), DynOb(sf("dom")), DynOb(sf("codom")))
)

case class DynAttrType(name: String) {
  override def toString() = name
}

given ReadWriter[DynAttrType] = macroRW

case class DynAttr(name: String, dom: DynOb, codom: DynAttrType)
    extends AttrWithDom {
  override def toString() = name
  type Value = ujson.Value
  val rw = summon[ReadWriter[ujson.Value]]
}

given ReadWriter[DynAttr] = readwriter[Map[String, String]].bimap[DynAttr](
  f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
  sf => DynAttr(sf("name"), DynOb(sf("dom")), DynAttrType(sf("codom")))
)

case class VersionSpec(
    @upickle.implicits.key("ACSetSchema")
    acsetSchema: String,
    @upickle.implicits.key("Catlab")
    catlab: String
)

given ReadWriter[VersionSpec] = macroRW

case class DynSchema(
    @upickle.implicits.key("Ob")
    obs: Seq[DynOb],
    @upickle.implicits.key("Hom")
    allhoms: Seq[DynHom],
    @upickle.implicits.key("AttrType")
    attrtypes: Seq[DynAttrType],
    @upickle.implicits.key("Attr")
    allattrs: Seq[DynAttr]
) {
  val obsByString = obs.map(ob => (ob.toString(), ob)).toMap
  val homsByString = allhoms.map(f => (f.toString(), f)).toMap
  val attrsByString = allattrs.map(f => (f.toString(), f)).toMap

  def readACSet(
      catlabJson: Map[String, Seq[Map[String, ujson.Value]]]
  ): (ACSet[DynSchema], Map[(Ob, Int), Part]) = {
    val buf = MutableACSet(this)
    val partMaps = mutable.Map[(Ob, Int), Part]()
    for (ob <- obs) {
      for (i <- 0 to (catlabJson(ob.name).length - 1)) {
        // Add one because Julia has 1-based indexing
        partMaps.put((ob, i + 1), buf.addPart(ob))
      }
    }
    for (ob <- obs) {
      for ((subparts, i) <- catlabJson(ob.name).zipWithIndex) {
        val x = partMaps((ob, i + 1))
        for (f <- allhoms.filter(_.dom == ob)) {
          if (subparts contains f.name) {
            buf.setSubpart(
              f,
              x,
              partMaps((f.codom, read[Int](subparts(f.name))))
            )
          }
        }
        for (f <- allattrs.filter(_.dom == ob)) {
          if (subparts contains f.name) {
            buf.setSubpart(f, x, read[f.Value](subparts(f.name)))
          }
        }
      }
    }

    (buf.freeze, partMaps.toMap)
  }

  def readACSet(s: String): (ACSet[DynSchema], Map[(Ob, Int), Part]) =
    readACSet(
      read[Map[String, Seq[Map[String, ujson.Value]]]](s)
    )
}

object DynSchema {
  def apply() = new DynSchema(Seq(), Seq(), Seq(), Seq())
}

given ReadWriter[DynSchema] = macroRW

given IsSchema[DynSchema] = new IsSchema[DynSchema] {
  extension (s: DynSchema)
    def obs = s.obs
    def homs(x: Ob) = s.allhoms.filter(_.dom == x)
    def attrs(x: Ob) = s.allattrs.filter(_.dom == x)
    def globals = Seq()

    def obsByString = s.obsByString
    def homsByString = s.homsByString
    def attrsByString = s.attrsByString
    def globalsByString = Map()

  val rw = readwriter[DynSchema]
}

type DynACSet = ACSet[DynSchema]

object DynACSet {
  def apply(s: DynSchema) = ACSet[DynSchema](s)
}
