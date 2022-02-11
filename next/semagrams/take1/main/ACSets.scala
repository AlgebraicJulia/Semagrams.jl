package semagrams

import scala.collection.immutable._
import cats.data.State
import cats.data._
import monocle.syntax.all._
import monocle.macros.GenLens

object ACSets {
  case class Schema(
      val obs: Seq[String],
      val homs: Seq[String],
      val attrtypes: Seq[String],
      val attrs: Seq[String],
      val doms: Map[String, String],
      val codoms: Map[String, String]
  )

  case class Entity(ty: String, id: Int)

  case class FinDomFunction[A](
      map: HashMap[Int, A],
      index: Option[HashMap[A, HashSet[Int]]]
  ) {

    def set(i: Int, v: A): FinDomFunction[A] = {
      FinDomFunction[A](
        map + (i -> v),
        index.map(idx =>
          idx + (v -> (idx.getOrElse(v, HashSet[Int]()) + i))
        )
      )
    }

    def apply(i: Int): Option[A] = map.get(i)

    def unset(i: Int): FinDomFunction[A] = {
      FinDomFunction[A](
        map - i,
        index.map(idx =>
          apply(i) match {
            case Some(v) => idx + (v -> (idx(v) - i))
            case None => idx
          }
        )
      )
    }

    def inverse(v: A): HashSet[Int] = {
      index match {
        case Some(idx) => {
          idx.getOrElse(v, HashSet[Int]())
        }
        case None => {
          HashSet(map.iterator.filter({ case (i, vp) => vp == v }).map(_._1).toSeq: _*)
        }
      }
    }

    override def toString(): String = {
      s"FinDomFunction($map, $index)"
    }
  }

  object FinDomFunction {
    def apply[A](indexed: Boolean): FinDomFunction[A] = {
      this(
        HashMap[Int, A](),
        if indexed then Some(HashMap[A, HashSet[Int]]()) else None
      )
    }

  }

  case class IDGenerator(i: Int) {
    def next(): (IDGenerator, Int) = {
      (IDGenerator(i+1), i)
    }
  }

  object IDGenerator {
    def apply(): IDGenerator = IDGenerator(0)
  }

  case class ACSet[T](
    val schema: Schema,
    val idgen: IDGenerator,
    val parts: HashMap[String, HashMap[Int,T]],
    val hom_subparts: HashMap[String, FinDomFunction[Int]],
    val attr_subparts: HashMap[String, FinDomFunction[String]]
  ) {

    def add_part(ty: String, params: T): (ACSet[T], Entity) = {
      assert(schema.obs contains ty)
      val (newgen, i) = idgen.next()
      (this.copy(
         idgen = newgen,
         parts = parts + (ty -> (parts(ty) + (i -> params)))),
       Entity(ty, i))
    }

    def rem_part(e: Entity): ACSet[T] = {
      this.copy(
        parts = parts + (e.ty -> (parts(e.ty) - (e.id))),
        hom_subparts = HashMap[String, FinDomFunction[Int]](
          hom_subparts.map({ case (fname, f) =>
                             (fname, if schema.doms(fname) == e.ty then f.unset(e.id) else f)
                           }).toSeq: _*),
        attr_subparts = HashMap[String, FinDomFunction[String]](
          attr_subparts.map({ case (fname, f) =>
                             (fname, if schema.doms(fname) == e.ty then f.unset(e.id) else f)
                           }).toSeq: _*),
      )
    }

    def has_part(e: Entity): Boolean = parts(e.ty).contains(e.id)

    def get_parts(ty: String): Iterator[Entity] =
      parts(ty).keys.map(i => Entity(ty, i)).iterator

    def all_entities(): Iterator[Entity] =
      parts.keys.map(this.get_parts(_)).foldLeft(Seq[Entity]().iterator)(_++_)

    def entities_with_params(ty: String): Iterator[(Entity, T)] =
      parts(ty).map({ case (i, p) => (Entity(ty,i), p) }).iterator

    def entities_with_params(): Iterator[(Entity, T)] =
      parts.keys.map(this.entities_with_params(_)).foldLeft(Seq[(Entity,T)]().iterator)(_++_)

    def nparts(ty: String): Int = parts(ty).size

    def hom_subpart(e: Entity, f: String): Option[Entity] = {
      assert(schema.doms(f) == e.ty)
      for {
        v <- hom_subparts(f)(e.id)
      } yield Entity(schema.codoms(f), v)
    }

    def attr_subpart(e: Entity, f: String): Option[String] = {
      assert(schema.doms(f) == e.ty)
      attr_subparts(f)(e.id)
    }

    def set_hom_subpart(e: Entity, f: String, v: Entity): ACSet[T] = {
      assert(schema.doms(f) == e.ty && schema.codoms(f) == v.ty)
      this.copy(
        hom_subparts = hom_subparts + (f -> hom_subparts(f).set(e.id, v.id))
      )
    }

    def set_attr_subpart(e: Entity, f: String, v: String): ACSet[T] = {
      assert(schema.doms(f) == e.ty)
      this.copy(
        attr_subparts = attr_subparts + (f -> attr_subparts(f).set(e.id, v))
      )
    }

    def hom_incident(f: String, e: Entity): HashSet[Entity] = {
      assert(schema.codoms(f) == e.ty)
      hom_subparts(f).inverse(e.id).map(i => Entity(schema.doms(f), i))
    }

    def attr_incident(f: String, v: String): HashSet[Entity] = {
      attr_subparts(f).inverse(v).map(i => Entity(schema.doms(f), i))
    }

    override def toString(): String = {
      s"ACSet[T]($parts, $hom_subparts, $attr_subparts)"
    }
  }

  object ACSet {
    def apply[T](schema: Schema): ACSet[T] = {
      ACSet[T](
        schema,
        IDGenerator(),
        HashMap[String, HashMap[Int,T]](
          schema.obs.map(x => x -> HashMap[Int,T]()): _*
        ),
        HashMap[String, FinDomFunction[Int]](
          schema.homs.map(x => x -> FinDomFunction[Int](true)): _*
        ),
        HashMap[String, FinDomFunction[String]](
          schema.attrs.map(x => x -> FinDomFunction[String](false)): _*
        ),
      )
    }
  }

  def add_part[T](ty: String, params: T): State[ACSet[T], Entity] =
    State(_.add_part(ty, params))

  def set_hom_subpart[T](e: Entity, f: String, v: Entity): State[ACSet[T], Unit] =
    State.modify(_.set_hom_subpart(e, f, v))

  def set_attr_subpart[T](e: Entity, f: String, v: String): State[ACSet[T], Unit] =
    State.modify(_.set_attr_subpart(e, f, v))
}
