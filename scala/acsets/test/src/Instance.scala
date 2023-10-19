package acsets

import utest._

object InstanceTests extends TestSuite {
  val tests = Tests {
    val g = Instance.Dirty.empty(SchGraph)
    val (g1, v1) = g.addPart(V)
    val (g2, v2) = g1.addPart(V)
    val (g3, e1) = g2.addPart(E)
    val g4 = g3.setSubpart(e1, src, v1)
    val g5 = g4.setSubpart(e1, tgt, v2)

    test("basics") {
      assert(g5.parts contains v1.id)
      assert(g5.parts contains v2.id)
      assert(g5.subpart(e1, src) == Some(v1))
    }

    val wg = Instance.Dirty.empty(SchWeightedGraph)
    val (wg1, we1) = wg.addPart(E)
    val wg2 = wg1.setSubpart(we1, intWeight, 5)

    test("weighted") {
      assert(wg2.subpart(we1, intWeight) == Some(5))
    }
  }
}
