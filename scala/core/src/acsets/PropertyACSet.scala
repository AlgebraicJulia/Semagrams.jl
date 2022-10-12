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

case class WithProps[A: ACSet](acset: BareACSet) {
  def setProp[X <: Ob, T](v: Elt[X], p: Property[T], t: T): WithProps[A] = {
    val pm = this.subpart(Props(v.ty), v).get
    this.setSubpart(Props(v.ty), v, pm + (p, t))
  }

  def getProp[X <: Ob, T](v: Elt[X], p: Property[T]): T =
    this.subpart(Props(v.ty), v).get(p)

  def untypedGetProp[X <: Ob, T](v: Entity, p: Property[T]): T =
    this
      .untypedSubpart(Props(v.entityType.asInstanceOf[Ob]), v)
      .get
      .asInstanceOf[PropMap](p)
}

trait PropOps[A] extends ACSetOps[WithProps[A]] {
  def addPartWP[X <: Ob](x: X, pm: PropMap): State[WithProps[A], Elt[X]] =
    for {
      v <- addPart(x)
      _ <- setSubpart(Props(x), v, pm)
    } yield v

  def setProp[X <: Ob, T](
    v: Elt[X],
    p: Property[T],
    t: T
  ): State[WithProps[A], Unit] =
    State.modify(_.setProp(v, p, t))
}

object WithProps {
  def ops[A: ACSet] = new PropOps[A] {
    given acsetInstance: ACSet[WithProps[A]] with
      val bare = Iso[WithProps[A], BareACSet](_.acset)(new WithProps[A](_))
      val aAcset = summon[ACSet[A]]
      val schema = aAcset.schema.copy(
        attrs = aAcset.schema.attrs ++
          aAcset.schema.obs.values
            .map(ob => {
              val p = Props(ob)
              (p.toString.toLowerCase(), p)
            })
            .toMap
      )

  }

  def apply[A: ACSet]() = ops[A].acsetInstance.empty
}

given [A: ACSet]: ACSet[WithProps[A]] = WithProps.ops[A].acsetInstance
