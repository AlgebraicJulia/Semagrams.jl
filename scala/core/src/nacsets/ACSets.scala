package semagrams.nacsets

import semagrams._
import cats.data.NonEmptyList

trait Ob

trait Hom extends Property {
  val dom: Seq[Ob]
  val codom: Seq[Ob]
}

trait Attr {
  val dom: Ob
  type codom
}

trait Subschema {
  val indexBy: Seq[Ob]
  val schema: NestedSchema
}

case class PartType(path: List[Subschema]) extends EntityType {
  def extend(x: Subschema) = PartType(path :+ x)
}

case class NestedSchema(
  obs: Seq[Ob],
  homs: Seq[Hom],
  attrs: Seq[Attr],
  subschemas: Seq[Subschema]
) {
  def subschema(ty: PartType): NestedSchema = ty.path match {
    case Nil => this
    case s :: rest => {
      assert(s.indexBy.map(obs.contains).foldLeft(true)(_ && _))
      s.schema.subschema(PartType(rest))
    }
  }
}


case class Id(id: Int)

case class PartData(
  nextId: Int,
  ids: Seq[Id],
) {
  def addPart(): (PartData, Id) = {
    val i = Id(nextId)
    val newPd = PartData(
      nextId + 1,
      ids :+ i,
    )
    (newPd, i)
  }
}


// The empty list refers to the acset itself
case class Part(path: List[(Subschema, Seq[Id])]) extends Entity {
  val ty = PartType(path.map(_._1))

  def extend(s: Subschema, ids: Seq[Id]) = Part(path :+ (s, ids))
}

val ROOT = Part(List())

case class ACSet(
  schema: NestedSchema,
  globals: PropMap,
  partdata: Map[Ob, PartData],
  subacsets: Map[Subschema, Map[Seq[Id], ACSet]]
) {
  def subacset(p: Part): ACSet = p.path match {
    case Nil => this
    case (s,ids)::rest => subacsets(s)(ids).subacset(Part(rest))
  }

  def setSubacset(p: Part, acs: ACSet): ACSet = p.path match {
    case Nil => {
      acs
    }
    // We special case this so that we don't have to make a new acset just to see it replaced immediately
    case (s, ids)::Nil => {
      val subacsetMap = subacsets.get(s).getOrElse(Map())
      this.copy(
        subacsets = subacsets + (s -> (subacsetMap + (ids -> acs)))
      )
    }
    case (s,ids)::rest => {
      val subacsetMap = subacsets.get(s).getOrElse(Map())
      val sub = subacsetMap.get(ids).getOrElse(ACSet(s.schema))
      this.copy(
        subacsets = subacsets + (s -> (subacsetMap + (ids -> sub.setSubacset(Part(rest), acs))))
      )
    }
  }

  def parts(i: Part, x: Ob): Seq[Id] = subacset(i).partdata(x).ids

  def subpart(f: Property, i: Part): f.Value = subacset(i).globals(f)

  def addPart(p: Part, x: Ob): (ACSet, Part) = {
    val sub = subacset(p)
    val (newPd, i) = sub.partdata(x).addPart()
    val newSub = sub.copy(
      partdata = sub.partdata + (x -> newPd)
    )
    (setSubacset(p, newSub), p.extend(x,i))
  }

  def setSubpart(p: Part, f: Property, v: f.Value): ACSet = {
    val sub = subacset(p)
    val newSub = sub.copy(
      globals = sub.globals.set(f, v)
    )
    setSubacset(p, newSub)
  }

  def incident(p: Part, f: Hom): Seq[Part] = {

  }
}

object ACSet {
  def apply(s: NestedSchema) =
    new ACSet(s, PropMap(), s.obs.map(ob => PartData(0, Seq(), Map())).toMap)
}
