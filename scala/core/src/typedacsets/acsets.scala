package semagrams.typedacsets

trait HasVal {
  type Val
}

trait HasProperties {
  type Property <: HasVal
}

trait Schema {
  type Ob <: HasProperties

  case class Part[X <: Ob](ob: X, i: Int)

  case class ACSet(
    parts: Map[Ob, Map[Int, Map[Any, Any]]],
    next: Int
  ) {
    def addPart[X <: Ob](ob: X): (ACSet, Part[X]) = {
      (
        this.copy(
          parts = parts + (ob -> (parts(ob) + (next -> Map()))),
          next = next + 1
        ),
        Part[X](ob, next)
      )
    }

    def setSubpart[X <: Ob](p: Part[X], f: p.ob.Property, v: f.Val): ACSet = {
      this.copy(
        parts = parts + (p.ob -> (parts(p.ob) + (p.i -> (parts(p.ob)(p.i) + (f -> v)))))
      )
    }

    def subpart[X <: Ob](p: Part[X], f: p.ob.Property): f.Val = {
      parts(p.ob)(p.i)(f).asInstanceOf[f.Val]
    }
  }
}

object DynSchema extends Schema {
  case class Ob(name: String) extends HasProperties {
    case class Property(name: String) extends HasVal {
      type Val = String
    }
  }
}

val a = DynSchema.ACSet(Map() + (DynSchema.Ob("X") -> Map()), 0)
val (a1, x) = a.addPart(DynSchema.Ob("X"))
val a2 = a1.setSubpart(x, x.ob.Property("label"), "green")

object SchGraph extends Schema {
  sealed trait Ob extends HasProperties

  case object E extends Ob {
    sealed trait Property extends HasVal

    case object Src extends Property {
      type Val = Part[V.type]
    }
  }

  case object V extends Ob {
    sealed trait Property extends HasVal
  }
}

val g = SchGraph.ACSet(Map() + (SchGraph.E -> Map()) + (SchGraph.V -> Map()), 0)
val (g1, v) = g.addPart(SchGraph.V)
val (g2, e) = g1.addPart(SchGraph.E)
val g3 = g2.setSubpart(e, e.ob.Src, v)
