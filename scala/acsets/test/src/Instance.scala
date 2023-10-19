package acsets

import utest._

object InstanceTests extends TestSuite {
  val tests = Tests {
    val g = Instance.Dirty.empty(SchGraph)
    val (g1, v1) = g.addPart(V)
    val (g2, v2) = g1.addPart(V)
    val (g3, e1) = g2.addPart(E)
    val g4 = g3.setSubpart(e1, src, Value.Reference(v1))
    val g5 = g4.setSubpart(e1, tgt, Value.Reference(v2))

    test("basics") {
      assert(g5.parts contains v1.id)
      assert(g5.parts contains v2.id)
      assert(g5.subpart(e1, src) == Some(Value.Reference(v1)))
    }
  }
}
