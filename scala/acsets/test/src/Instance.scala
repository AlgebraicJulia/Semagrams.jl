package acsets

import utest._

object InstanceTests extends TestSuite {
  val tests = Tests {
    import Instance._

    val g = Dirty.empty(SchGraph)

    val v1 = g.addPart(V)
    val v2 = g.addPart(V)
    val e1 = g.addPart(E)
    g.setSubpart(e1, src, v1)
    g.setSubpart(e1, tgt, v2)

    test("dirty accessors") {
      assert(g.parts contains v1.id)
      assert(g.parts contains v2.id)
      assert(g.subpart(e1, src) == Some(v1))
    }

    val patch = Instance.commit(g).get
    val g1 = Instance.applyPatch(g.clean, patch).get

    test("clean accessors") {
      assert(g1.parts contains v1.id)
      assert(g1.parts contains v2.id)
      assert(g1.subpart(e1, src) == Some(v1))
    }

    val wg = Dirty.empty(SchWeightedGraph)

    val we1 = wg.addPart(E)
    wg.setSubpart(we1, intWeight, 5)

    test("weighted") {
      assert(wg.subpart(we1, intWeight) == Some(5))
    }
  }
}
