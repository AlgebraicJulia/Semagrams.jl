package semagrams.acsets

import monocle.syntax.all._
import monocle.macros.GenLens
import monocle.macros.GenIso
import monocle._
import monocle.function.Index
import cats.implicits._
import cats.data.State

case class Entity[Ob](id: Int)

abstract class Ob
abstract class Hom[Dom <: Ob, Codom <: Ob]
abstract class Attr[Dom <: Ob, Codom]

case class Schema(obs: List[Ob], homs: List[Hom[Ob, Ob]], attrs: List[Attr[Ob, Any]])

object Schema {
  def apply(args: Any*) = {
    val obs = args.collect {
      case (x: Ob) => x
    }
    val homs = args.collect {
      case (f: Hom[?, ?]) => f.asInstanceOf[Hom[Ob,Ob]]
    }
    val attrs = args.collect {
      case (f: Attr[?, ?]) => f.asInstanceOf[Attr[Ob,Any]]
    }
    new Schema(obs.toList, homs.toList, attrs.toList)
  }
}

case class BareACSet(
  _nextId: Int,
  _obs: Map[Ob, Set[Entity[Ob]]],
  _homs: Map[Hom[Ob,Ob], Map[Entity[Ob], Entity[Ob]]],
  _attrs: Map[Attr[Ob,Any], Map[Entity[Ob], Any]]
) {
  val nextId = GenLens[BareACSet](_._nextId)
  val obs = GenLens[BareACSet](_._obs)
  val homs = GenLens[BareACSet](_._homs)
  val attrs = GenLens[BareACSet](_._attrs)

  def parts[X <: Ob](s: Schema, ob: X): Set[Entity[X]] = {
    assert(s.obs contains ob)
    _obs(ob).asInstanceOf[Set[Entity[X]]]
  }

  def addPart[X <: Ob](s: Schema, ob: X): (Entity[X], BareACSet) = {
    assert(s.obs contains ob)
    val e = Entity[Ob](_nextId)
    (
      e.asInstanceOf[Entity[X]],
      nextId.modify(_ + 1)(obs.modify(_.focus(_.index(ob)).modify(_ + e))(this))
    )
  }

  def subpart[X <: Ob, Y <: Ob](s: Schema, f: Hom[X, Y], x: Entity[X]): Option[Entity[Y]] = {
    assert(s.homs contains f)
    _homs(f.asInstanceOf[Hom[Ob,Ob]]).asInstanceOf[Map[Entity[X], Entity[Y]]].get(x)
  }

  def subpart[X <: Ob, T](s: Schema, f: Attr[X, T], x: Entity[X]): Option[T] = {
    assert(s.attrs contains f)
    _attrs(f.asInstanceOf[Attr[Ob,Any]]).asInstanceOf[Map[Entity[X], T]].get(x)
  }

  def setSubpart[X <: Ob, Y <: Ob](s: Schema, f: Hom[X, Y], x: Entity[X], y: Entity[Y]): BareACSet = {
    assert(s.homs contains f)
    val xp = x.asInstanceOf[Entity[Ob]]
    val yp = y.asInstanceOf[Entity[Ob]]
    val fp = f.asInstanceOf[Hom[Ob, Ob]]
    homs.modify(_.focus(_.index(fp)).modify(_ + (xp -> yp)))(this)
  }

  def setSubpart[X <: Ob, T](s: Schema, f: Attr[X, T], x: Entity[X], y: T): BareACSet = {
    assert(s.attrs contains f)
    val xp = x.asInstanceOf[Entity[Ob]]
    val fp = f.asInstanceOf[Attr[Ob, Any]]
    attrs.modify(_.focus(_.index(fp)).modify(_ + (xp -> y)))(this)
  }
}

object BareACSet {
  def apply(s: Schema) = {
    new BareACSet(
      0,
      s.obs.map(ob => (ob -> Set[Entity[Ob]]())).toMap,
      s.homs.map(f => (f -> Map[Entity[Ob], Entity[Ob]]())).toMap,
      s.attrs.map(f => (f -> Map[Entity[Ob], Any]())).toMap,
    )
  }
}

trait ACSet[A] {
  val bare: Iso[A, BareACSet]
  val schema: Schema

  def empty: A = {
    bare.reverseGet(BareACSet(schema))
  }

  extension(a:A)
    def parts[X <: Ob](ob: X): Set[Entity[X]] = {
      bare.get(a).parts(schema, ob)
    }

    def addPart[X <: Ob](ob: X): (Entity[X], A) = {
      bare.modifyF(_.addPart(schema, ob))(a)
    }

    def subpart[X <: Ob, Y <: Ob](f: Hom[X, Y], x: Entity[X]): Option[Entity[Y]] = {
      bare.get(a).subpart(schema, f, x)
    }

    def subpart[X <: Ob, T](f: Attr[X, T], x: Entity[X]): Option[T] = {
      bare.get(a).subpart(schema, f, x)
    }

    def setSubpart[X <: Ob, Y <: Ob](f: Hom[X, Y], x: Entity[X], y: Entity[Y]): A = {
      bare.modify(_.setSubpart(schema, f, x, y))(a)
    }

    def setSubpart[X <: Ob, T](f: Attr[X, T], x: Entity[X], y: T): A = {
      bare.modify(_.setSubpart(schema, f, x, y))(a)
    }
}

def addPart[A: ACSet, X <: Ob](ob: X): State[A, Entity[X]] =
  State(_.addPart(ob).swap)

def setSubpart[A: ACSet, X <: Ob, Y <: Ob](f: Hom[X, Y], x: Entity[X], y: Entity[Y]): State[A,Unit] =
  State.modify(_.setSubpart(f, x, y))

def setSubpart[A: ACSet, X <: Ob, T](f: Attr[X, T], x: Entity[X], y: T): State[A,Unit] =
  State.modify(_.setSubpart(f, x, y))
