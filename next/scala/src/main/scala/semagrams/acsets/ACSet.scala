package semagrams.acsets

import monocle.syntax.all._
import monocle.macros.GenLens
import monocle.macros.GenIso
import monocle._
import monocle.function.Index
import cats.implicits._
import cats.data.State

import semagrams.Entity

case class Elt[Ob](id: Int) extends Entity

abstract class Ob

abstract class GenHom
abstract class Hom[Dom <: Ob, Codom <: Ob] extends GenHom

abstract class GenAttr
abstract class Attr[Dom <: Ob, Codom] extends GenAttr

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
  _obs: Map[Ob, Set[Entity]],
  _homs: Map[GenHom, Map[Entity, Entity]],
  _attrs: Map[GenAttr, Map[Entity, Any]]
) {
  val nextId = GenLens[BareACSet](_._nextId)
  val obs = GenLens[BareACSet](_._obs)
  val homs = GenLens[BareACSet](_._homs)
  val attrs = GenLens[BareACSet](_._attrs)

  def parts[X <: Ob](s: Schema, ob: X): Set[Elt[X]] = {
    assert(s.obs contains ob)
    _obs(ob).asInstanceOf[Set[Elt[X]]]
  }

  def addPart[X <: Ob](s: Schema, ob: X): (Elt[X], BareACSet) = {
    assert(s.obs contains ob)
    val e = Elt[X](_nextId)
    (
      e,
      nextId.modify(_ + 1)(obs.modify(_.focus(_.index(ob)).modify(_ + e))(this))
    )
  }

  def subpart[X <: Ob, Y <: Ob](s: Schema, f: Hom[X, Y], x: Elt[X]): Option[Elt[Y]] = {
    assert(s.homs contains f)
    _homs(f.asInstanceOf[Hom[Ob,Ob]]).asInstanceOf[Map[Elt[X], Elt[Y]]].get(x)
  }

  def subpart[X <: Ob, T](s: Schema, f: Attr[X, T], x: Elt[X]): Option[T] = {
    assert(s.attrs contains f)
    _attrs(f.asInstanceOf[Attr[Ob,Any]]).asInstanceOf[Map[Elt[X], T]].get(x)
  }

  def setSubpart[X <: Ob, Y <: Ob](s: Schema, f: Hom[X, Y], x: Elt[X], y: Elt[Y]): BareACSet = {
    assert(s.homs contains f)
    val xp = x.asInstanceOf[Elt[Ob]]
    val yp = y.asInstanceOf[Elt[Ob]]
    val fp = f.asInstanceOf[Hom[Ob, Ob]]
    homs.modify(_.focus(_.index(fp)).modify(_ + (xp -> yp)))(this)
  }

  def setSubpart[X <: Ob, T](s: Schema, f: Attr[X, T], x: Elt[X], y: T): BareACSet = {
    assert(s.attrs contains f)
    val xp = x.asInstanceOf[Elt[Ob]]
    val fp = f.asInstanceOf[Attr[Ob, Any]]
    attrs.modify(_.focus(_.index(fp)).modify(_ + (xp -> y)))(this)
  }
}

object BareACSet {
  def apply(s: Schema) = {
    new BareACSet(
      0,
      s.obs.map(ob => (ob -> Set[Entity]())).toMap,
      s.homs.map(f => (f -> Map[Entity, Entity]())).toMap,
      s.attrs.map(f => (f -> Map[Entity, Any]())).toMap,
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
    def parts[X <: Ob](ob: X): Set[Elt[X]] = {
      bare.get(a).parts(schema, ob)
    }

    def addPart[X <: Ob](ob: X): (Elt[X], A) = {
      bare.modifyF(_.addPart(schema, ob))(a)
    }

    def subpart[X <: Ob, Y <: Ob](f: Hom[X, Y], x: Elt[X]): Option[Elt[Y]] = {
      bare.get(a).subpart(schema, f, x)
    }

    def subpart[X <: Ob, T](f: Attr[X, T], x: Elt[X]): Option[T] = {
      bare.get(a).subpart(schema, f, x)
    }

    def setSubpart[X <: Ob, Y <: Ob](f: Hom[X, Y], x: Elt[X], y: Elt[Y]): A = {
      bare.modify(_.setSubpart(schema, f, x, y))(a)
    }

    def setSubpart[X <: Ob, T](f: Attr[X, T], x: Elt[X], y: T): A = {
      bare.modify(_.setSubpart(schema, f, x, y))(a)
    }
}

def addPart[A: ACSet, X <: Ob](ob: X): State[A, Elt[X]] =
  State(_.addPart(ob).swap)

def setSubpart[A: ACSet, X <: Ob, Y <: Ob](f: Hom[X, Y], x: Elt[X], y: Elt[Y]): State[A,Unit] =
  State.modify(_.setSubpart(f, x, y))

def setSubpart[A: ACSet, X <: Ob, T](f: Attr[X, T], x: Elt[X], y: T): State[A,Unit] =
  State.modify(_.setSubpart(f, x, y))
