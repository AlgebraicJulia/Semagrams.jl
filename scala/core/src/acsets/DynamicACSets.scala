package semagrams.acsets

import upickle.default._
import scala.collection.mutable

case class SymOb(name: String) extends Ob {
  override def toString() = name
}

given ReadWriter[SymOb] = macroRW

case class SymHom(name: String, dom: SymOb, codom: SymOb) extends HomWithDom {
  override def toString() = name
}

given ReadWriter[SymHom] = readwriter[Map[String, String]].bimap[SymHom](
  f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
  sf => SymHom(sf("name"), SymOb(sf("dom")), SymOb(sf("codom")))
)

case class SymAttrType(name: String) {
  override def toString() = name
}

given ReadWriter[SymAttrType] = macroRW

case class SymAttr(name: String, dom: SymOb, codom: SymAttrType)
    extends AttrWithDom {
  override def toString() = name
  type Value = String
  val rw = summon[ReadWriter[String]]
}

given ReadWriter[SymAttr] = readwriter[Map[String, String]].bimap[SymAttr](
  f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
  sf => SymAttr(sf("name"), SymOb(sf("dom")), SymAttrType(sf("codom")))
)

case class VersionSpec(
    @upickle.implicits.key("ACSetSchema")
    acsetSchema: String,
    @upickle.implicits.key("Catlab")
    catlab: String
)

given ReadWriter[VersionSpec] = macroRW

case class DynamicSchema(
    version: VersionSpec,
    @upickle.implicits.key("Ob")
    obs: Seq[SymOb],
    @upickle.implicits.key("Hom")
    allhoms: Seq[SymHom],
    @upickle.implicits.key("AttrType")
    attrtypes: Seq[SymAttrType],
    @upickle.implicits.key("Attr")
    allattrs: Seq[SymAttr]
) {
  val obsByString = obs.map(ob => (ob.toString(), ob)).toMap
  val homsByString = allhoms.map(f => (f.toString(), f)).toMap
  val attrsByString = allattrs.map(f => (f.toString(), f)).toMap

  def readACSet(
      catlabJson: Map[String, Seq[Map[String, ujson.Value]]]
  ): ACSet[DynamicSchema] = {
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
            buf.setSubpart(f, x, read[String](subparts(f.name)))
          }
        }
      }
    }

    buf.freeze
  }

  def readACSet(s: String): ACSet[DynamicSchema] = readACSet(
    read[Map[String, Seq[Map[String, ujson.Value]]]](s)
  )
}

given ReadWriter[DynamicSchema] = macroRW

given IsSchema[DynamicSchema] = new IsSchema[DynamicSchema] {
  extension (s: DynamicSchema)
    def obs = s.obs
    def homs(x: Ob) = s.allhoms.filter(_.dom == x)
    def attrs(x: Ob) = s.allattrs.filter(_.dom == x)

    def obsByString = s.obsByString
    def homsByString = s.homsByString
    def attrsByString = s.attrsByString

  val rw = readwriter[DynamicSchema]
}

type DynACSet = ACSet[DynamicSchema]

object DynACSet {
  def apply(s: DynamicSchema) = ACSet[DynamicSchema](s)
}
