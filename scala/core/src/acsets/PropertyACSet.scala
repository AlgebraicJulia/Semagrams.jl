package semagrams.acsets

import semagrams._
import semagrams.sprites._
import monocle.macros.GenIso
import monocle._
import cats.data.State

case object PropValue extends AttrType {
  type Value = PropMap
}

case class Props[X <: Ob](x: X) extends Attr[X, PropMap] {
  val dom = x
  val codom = PropValue
}


trait PropOps[A: ACSet: HasProps] extends ACSetOps[A] {
  def addPartWP[X <: Ob](x: X, pm: PropMap): State[A, Elt[X]] =
    for {
      v <- addPart(x)
      _ <- setSubpart(Props(x), v, pm)
    } yield v

  def setProp[X <: Ob, T](
      v: Elt[X],
      p: Property[T],
      t: T
  ): State[A, Unit] =
    State.modify(_.setProp(v, p, t))
}

trait HasProps[A: ACSet] {
  extension (a: A)
    def setProp[X <: Ob, T](v: Elt[X], p: Property[T], t: T): A = {
      val pm = a.subpart(Props(v.ty), v).get
      a.setSubpart(Props(v.ty), v, pm + (p, t))
    }

    def getProp[X <: Ob, T](v: Elt[X], p: Property[T]): T =
      a.subpart(Props(v.ty), v).get(p)

    def untypedGetProp[X <: Ob, T](v: Entity, p: Property[T]): T =
      a.untypedSubpart(Props(v.entityType.asInstanceOf[Ob]), v)
        .get
        .asInstanceOf[PropMap](p)
}

object HasProps {
  def ops[A: ACSet: HasProps] = new PropOps[A] {}
}

trait StaticHasProps[A] extends StaticACSet[A] with HasProps[A]

def addPropsToSchema(s: Schema) =
  s.copy(
    attrs = s.attrs ++
      s.obs.values
        .map(ob => {
          val p = Props(ob)
          (p.toString.toLowerCase(), p)
        })
        .toMap
  )

case class WithProps[A: ACSet](a: A, s: Schema)

object WithProps {
  def ops[A: ACSet] = new PropOps[WithProps[A]] {
    given acsetInstance: ACSet[WithProps[A]] with
      val aAcset = summon[ACSet[A]]
      val bare =
        Lens[WithProps[A], A](_.a)(a => s => s.copy(a = a)).andThen(aAcset.bare)
      def getschema(a: WithProps[A]) = a.s

  }

  def apply[A: ACSet](a: A) =
    new WithProps[A](a: A, addPropsToSchema(a.schemaInst))
}

given [A: ACSet]: ACSet[WithProps[A]] = WithProps.ops[A].acsetInstance
given[A: ACSet]: HasProps[WithProps[A]] = new HasProps[WithProps[A]] {}

case class StaticWithProps[A: StaticACSet](a: A)

trait StaticPropOps[A] extends StaticACSetOps[A] with PropOps[A]

object StaticWithProps {
  def ops[A: StaticACSet] = new StaticPropOps[StaticWithProps[A]] {
    given acsetInstance: StaticACSet[StaticWithProps[A]] with
      val aAcset = summon[StaticACSet[A]]
      val bare = Iso[StaticWithProps[A], A](_.a)(StaticWithProps[A](_)).andThen(aAcset.bare)
      val schema = addPropsToSchema(aAcset.schema)
  }

  def apply[A: ACSet](a: A) =
    new WithProps[A](a: A, addPropsToSchema(a.schemaInst))
}

given[A: StaticACSet]: StaticACSet[StaticWithProps[A]] = StaticWithProps.ops[A].acsetInstance
