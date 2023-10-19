package acsets

import utest._

object SchemaTests extends TestSuite {
  val tests = Tests {
    val schEmpty = Schema.Clean.empty
    val V = SortId("V")
    val schVertexDirty = Schema.Dirty(schEmpty).addSort(V)
    val vertexPatch = schVertexDirty.patch

    test("vertex patch") {
      assert(schVertexDirty.sorts contains V)
      assert(Schema.commit(schVertexDirty).isDefined)
    }

    val schVertex = Schema.applyPatch(schEmpty, vertexPatch).get

    test("schema with vertex") {
      assert(schVertex.sorts contains V)
    }

    val E = SortId("V")
    val src = PropId("src")
    val tgt = PropId("tgt")

    val schGraphDirty = Schema
      .Dirty(schVertex)
      .addSort(E)
      .addProp(src, ValueType.Reference(V))
      .addProp(tgt, ValueType.Reference(V))
      .addMethod(E, src)
      .addMethod(E, tgt)

    test("edge patch") {
      assert(schGraphDirty.sorts contains E)
      assert(schGraphDirty.props contains src)
      assert(schGraphDirty.sorts contains V)
      assert(schGraphDirty.methods contains Method(E, src))
      assert(Schema.commit(schVertexDirty).isDefined)
    }

    val schGraph = Schema.applyPatch(schVertex, schGraphDirty.patch).get

    test("graph schema") {
      assert(schGraph.sorts contains E)
      assert(schGraph.props contains src)
      assert(schGraph.sorts contains V)
      assert(schGraph.methods contains Method(E, src))
    }
  }
}
