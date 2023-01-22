package semagrams.acsets

import semagrams._
import cats.data.State
import scala.collection.mutable
import upickle.default._

/**
 * In this file, we define nested ACSets.
 *
 * In a nested acset, each part of an acset has an associated acset.
 * The schema for a nested acset has an acset schema associated with each object,
 * which defines the schema for the acsets associated to the parts assigned to that object.
 *
 * A nested part of a nested acset is a list of (ob, id) pairs, which at each step tell you how to get
 * to the next nested acset.
 *
 * Morphisms in a nested acset schema go from nested parts to nested parts. The acset schema
 * that contains the morphism is the "highest point" that the morphism can go, i.e. the only way to
 * get from a nested part to another nested part via a morphism is to go all the way up to where the
 * morphism is defined, and then go back down to the target of the morphism.
 */

trait Ob {
  val schema: Schema = SchEmpty
}

trait Hom extends Property {
  val dom: PartType
  val codom: PartType

  type Value = Part

  val rw = summon[ReadWriter[String]].bimap(_.toString, _ => ???)
}

trait Attr {
  val dom: Seq[Ob]
  type codom
}

case class PartType(path: Seq[Ob]) extends EntityType {
  def extend(x: Ob) = PartType(path :+ x)
}

trait Schema {
  val obs: Seq[Ob]
  val homs: Seq[Hom]
  val attrs: Seq[Attr]

  def subschema(ty: PartType): Schema = ty.path match {
    case Nil => this
    case ob :: rest => {
      assert(obs contains ob)
      ob.schema.subschema(PartType(rest))
    }
  }

  // Returns all of the homs that go into the given part type
  // Each hom is prefixed by a path of objects needed to get to that hom
  def homsInto(ty: PartType): Seq[(Seq[Ob], Hom)] = ty.path match {
    case Nil => Seq()
    case ob::rest =>
      homs.filter(_.codom == ty).map((Seq(), _))
        ++ ob.schema.homsInto(PartType(rest)).map({ case (obs, f) => (ob +: obs, f) })
  }
}


case class Id(id: Int)

case class Parts(
  nextId: Int,
  ids: Seq[Id],
  acsets: Map[Id, ACSet]
) {
  def addPart(a: ACSet): (Parts, Id) = {
    val i = Id(nextId)
    val newPd = Parts(
      nextId + 1,
      ids :+ i,
      acsets + (i -> a)
    )
    (newPd, i)
  }

  def setAcset(i: Id, acs: ACSet) = {
    this.copy(
      acsets = acsets + (i -> acs)
    )
  }

  def remPart(i: Id) = {
    this.copy(
      ids = ids.filterNot(_ == i),
      acsets = acsets.filterNot(_._1 == i)
    )
  }
}


// The empty list refers to the acset itself
case class Part(path: Seq[(Ob, Id)]) extends Entity {
  override val ty: PartType = PartType(path.map(_._1))

  def extend(x: Ob, i: Id) = Part(path :+ (x, i))
}

val ROOT = Part(Seq())

case object SchEmpty extends Schema {
  val obs = Seq()
  val homs = Seq()
  val attrs = Seq()
}

case class ACSet(
  schema: Schema,
  props: PropMap,
  partsMap: Map[Ob, Parts],
) {
  def subacset(p: Part): ACSet = trySubacset(p).get

  def trySubacset(p: Part): Option[ACSet] = p.path match {
    case Nil => Some(this)
    case (x,i)::rest => partsMap.get(x).flatMap(_.acsets.get(i).flatMap(_.trySubacset(Part(rest))))
  }

  def hasPart(p: Part): Boolean = p.path match {
    case Nil => true
    case (x,i)::rest => (for {
      parts <- partsMap.get(x)
      sub <- parts.acsets.get(i)
      res <- Some(sub.hasPart(Part(rest)))
    } yield res).getOrElse(false)
  }

  def setSubacset(p: Part, acs: ACSet): ACSet = p.path match {
    case Nil => {
      acs
    }
    case (x, i)::rest => {
      val parts = partsMap(x)
      this.copy(
        partsMap = partsMap + (x -> (parts.setAcset(i, parts.acsets(i).setSubacset(Part(rest), acs))))
      )
    }
  }

  def parts(i: Part, x: Ob): Seq[Id] = subacset(i).partsMap(x).ids

  def subpart(f: Property, i: Part): f.Value = subacset(i).props(f)

  def trySubpart(f: Property, i: Part): Option[f.Value] = trySubacset(i).flatMap(_.props.get(f))

  def addPart(p: Part, x: Ob, props: PropMap): (ACSet, Part) = {
    val sub = subacset(p)
    val subschema = schema.subschema(p.ty.asInstanceOf[PartType].extend(x))
    val (newparts, i) = sub.partsMap(x).addPart(ACSet(subschema, props))
    val newSub = sub.copy(
      partsMap = sub.partsMap + (x -> newparts)
    )
    (setSubacset(p, newSub), p.extend(x,i))
  }

  def addPart(x: Ob, props: PropMap): (ACSet, Part) = addPart(ROOT, x, props)

  def addPart(p: Part, x: Ob): (ACSet, Part) = addPart(p, x, PropMap())

  def addPart(x: Ob): (ACSet, Part) = addPart(ROOT, x, PropMap())

  def setSubpart(p: Part, f: Property, v: f.Value): ACSet = {
    val sub = subacset(p)
    val newSub = sub.copy(
      props = sub.props.set(f, v)
    )
    setSubacset(p, newSub)
  }

  def remSubpart(p: Part, f: Property): ACSet = {
    val sub = subacset(p)
    val newSub = sub.copy(
      props = sub.props - f
    )
    setSubacset(p, newSub)
  }

  def incident(p: Part, f: Hom): Seq[Part] = {
    val prefix = Part(p.path.dropRight(f.codom.path.length))
    /**
     * Essentially, we look at all parts with part type f.dom, and filter which ones
     * have a property f set to p
     */
    def helper(acs: ACSet, part: Part, remaining: Seq[Ob]): Seq[Part] = remaining match {
      case Nil => if acs.props.get(f) == Some(p) then Seq(part) else Seq()
      case ob::rest =>
        acs.partsMap(ob).acsets.toSeq.flatMap((i, acs) => helper(acs, part.extend(ob, i), rest))
    }

    helper(subacset(prefix), prefix, f.dom.path)
  }

  def remPartOnly(p: Part): ACSet = {
    assert(p.path.length > 0)
    val (pre, (x, i)) = (p.path.dropRight(1), p.path.last)
    val sub = subacset(Part(pre))
    val newSub = sub.copy(
      partsMap = sub.partsMap + (x -> sub.partsMap(x).remPart(i))
    )
    setSubacset(Part(pre), newSub)
  }

  def remPart(p: Part): ACSet = {
    val visited = mutable.Set[Part]()
    val queue = mutable.Queue[Part](p)
    while (!queue.isEmpty) {
      val q = queue.dequeue()
      visited.add(q)
      for ((obs, f) <- schema.homsInto(q.ty)) {
        queue.enqueueAll(
          incident(q, f)
            .filter(!visited.contains(_))
        )
      }
    }
    val toRemove = visited.toSeq
    toRemove.foldLeft(this)(_.remPartOnly(_))
  }
}

object ACSet {
  def apply(s: Schema): ACSet = ACSet(s, PropMap())

  def apply(s: Schema, props: PropMap): ACSet =
    new ACSet(s, props, s.obs.map(ob => ob -> Parts(0, Seq(), Map())).toMap)

  def addPart(p: Part, x: Ob): State[ACSet, Part] = State(_.addPart(p,x))

  def addPart(x: Ob, props: PropMap): State[ACSet, Part] = State(_.addPart(x, props))

  def setSubpart(p: Part, f: Property, v: f.Value): State[ACSet, Unit] =
    State.modify(_.setSubpart(p, f, v))

  def remSubpart(p: Part, f: Property): State[ACSet, Unit] =
    State.modify(_.remSubpart(p, f))

  def remPart(p: Part): State[ACSet, Unit] = State.modify(_.remPart(p))
}
