package semagrams

import scala.collection.mutable;
import scala.collection.immutable;

object ACSets {
  case class Schema(
      val obs: Seq[Symbol],
      val homs: Seq[Symbol],
      val attrtypes: Seq[Symbol],
      val attrs: Seq[Symbol],
      val doms: Map[Symbol, Symbol],
      val codoms: Map[Symbol, Symbol]
  )

  case class Entity(ty: Symbol, id: Int)

  class FinDomFunction[A](
      indexed: Boolean
  ) {
    val map = new mutable.HashMap[Int, A]
    private val index = if (indexed) {
      Some(new mutable.HashMap[A, mutable.HashSet[Int]])
    } else {
      None
    }

    def set(i: Int, v: A): Unit = {
      map.addOne(i -> v)

      for {
        idx <- index
      } yield {
        val image = idx.getOrElseUpdate(v, new mutable.HashSet[Int])
        image.add(i)
      }
    }

    def apply(i: Int): Option[A] = map.get(i)

    def unset(i: Int): Unit = {
      for {
        v <- apply(i)
        idx <- index
        image <- idx.get(v)
      } yield image.remove(i)

      map.remove(i)
    }

    def inverse(v: A): List[Int] = {
      index match {
        case Some(idx) => {
          idx.get(v).toList.flatMap(image => image.iterator)
        }
        case None => {
          map.iterator.filter({ case (i, vp) => vp == v }).map(_._1).toList
        }
      }
    }

    override def toString(): String = {
      s"FinDomFunction($map, $index)"
    }
  }

  class IDGenerator {
    private var _i = 0

    def next = {
      _i = _i + 1
      _i
    }
  }

  class ACSet(
      val schema: Schema
  ) {
    private val idgen = new IDGenerator

    private val parts = immutable.HashMap[Symbol, mutable.HashSet[Int]](
      schema.obs.map(x => x -> new mutable.HashSet[Int]): _*
    )

    private val hom_subparts = immutable.HashMap[Symbol, FinDomFunction[Int]](
      schema.homs.map(f => f -> new FinDomFunction[Int](true)): _*
    )

    private val attr_subparts =
      immutable.HashMap[Symbol, FinDomFunction[String]](
        schema.attrs.map(f => f -> new FinDomFunction[String](false)): _*
      )

    def add_part(ty: Symbol): Entity = {
      val i = idgen.next
      parts(ty).add(i)
      new Entity(ty, i)
    }

    def rem_part(e: Entity): Unit = {
      parts(e.ty).remove(e.id)

      for {
        f <- schema.homs if schema.doms(f) == e.ty
      } yield hom_subparts(f).unset(e.id)

      for {
        f <- schema.attrs if schema.doms(f) == e.ty
      } yield hom_subparts(f).unset(e.id)
    }

    def has_part(e: Entity): Boolean = parts(e.ty).contains(e.id)

    def get_parts(ty: Symbol): Iterator[Entity] =
      parts(ty).iterator.map(i => new Entity(ty, i))

    def nparts(ty: Symbol): Int = parts(ty).size

    def hom_subpart(e: Entity, f: Symbol): Option[Entity] = {
      assert(schema.doms(f) == e.ty)
      for {
        v <- hom_subparts(f)(e.id)
      } yield new Entity(schema.codoms(f), v)
    }

    def attr_subpart(e: Entity, f: Symbol): Option[String] = {
      assert(schema.doms(f) == e.ty)
      attr_subparts(f)(e.id)
    }

    def set_hom_subpart(e: Entity, f: Symbol, v: Entity): Unit = {
      assert(schema.doms(f) == e.ty && schema.codoms(f) == v.ty)
      hom_subparts(f).set(e.id, v.id)
    }

    def set_attr_subpart(e: Entity, f: Symbol, v: String): Unit = {
      assert(schema.doms(f) == e.ty)
      attr_subparts(f).set(e.id, v)
    }

    override def toString(): String = {
      s"ACSet($parts, $hom_subparts, $attr_subparts)"
    }
  }
}
