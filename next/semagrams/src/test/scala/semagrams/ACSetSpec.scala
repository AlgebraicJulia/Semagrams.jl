package semagrams

import utest._
import semagrams.ACSets._
import scala.collection.immutable.HashMap

object ACSetSpec extends TestSuite {
  val GraphSchema = Schema(
    Vector('E, 'V),
    Vector('src, 'tgt),
    Vector(),
    Vector(),
    HashMap('src -> 'E, 'tgt -> 'E),
    HashMap('src -> 'V, 'tgt -> 'V)
  )

  def tests = Tests {
    test("empty graph") {
      val g = new ACSet(GraphSchema)
      assert(g.nparts('V) == 0)
      assert(g.nparts('E) == 0)
    }

    test("path graph") {
      val g = new ACSet(GraphSchema)
      val x = g.add_part('V)
      val y = g.add_part('V)
      val z = g.add_part('V)
      val k = g.add_part('E)
      val l = g.add_part('E)
      g.set_hom_subpart(k, 'src, x)
      g.set_hom_subpart(k, 'tgt, y)
      g.set_hom_subpart(l, 'src, y)
      g.set_hom_subpart(l, 'tgt, z)
      assert(g.nparts('V) == 3)
      assert(g.nparts('E) == 2)
      assert(g.get_parts('V).toList.contains(x))
      assert(g.get_parts('V).toList.contains(y))
      assert(g.get_parts('V).toList.contains(z))
      assert(g.has_part(x))
      assert(g.has_part(k))
      assert(g.hom_subpart(k, 'src) == Some(x))
      assert(g.hom_subpart(l, 'tgt) == Some(z))
    }
  }
}
