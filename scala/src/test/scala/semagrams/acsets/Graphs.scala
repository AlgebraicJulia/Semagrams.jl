package semagrams.acsets

import utest._
import semagrams.acsets._
import scala.collection.immutable.HashMap

object ACSetSpec extends TestSuite {
  def tests = Tests {
    test("empty graph") {
      val g = Graph()
      assert(g.vertices() == Set())
      assert(g.edges() == Set())
    }

    test("path graph") {
      val mkpath = for {
        x <- addVertex[Graph]()
        y <- addVertex()
        z <- addVertex()
        k <- addEdge(x, y)
        l <- addEdge(y, z)
      } yield (x, y, z, k, l)

      val (g, (x, y, z, k, l)) = mkpath.run(Graph()).value

      assert(g.vertices() contains x)
      assert(g.vertices() contains y)
      assert(g.vertices() contains z)
      assert(g.src(k) == Some(x))
      assert(g.tgt(l) == Some(z))
    }

    test("weighted graph") {
      val mkpath = for {
        x <- addVertex[WeightedGraph[String]]()
        y <- addVertex()
        z <- addVertex()
        k <- addEdge(x, y)
        l <- addEdge(y, z)
        _ <- setSubpart(Weight[String], k, "foo")
        _ <- setSubpart(Weight[String], l, "bar")
      } yield (x, y, z, k, l)

      val (g, (x, y, z, k, l)) = mkpath.run(WeightedGraph[String]()).value

      assert(g.subpart(Weight[String], k) == Some("foo"))
      assert(g.subpart(Weight[String], l) == Some("bar"))
    }

    test("incident") {
      val mkpath = for {
        x <- addVertex[Graph]()
        y <- addVertex()
        z <- addVertex()
        k <- addEdge(x, y)
        l <- addEdge(y, z)
      } yield (x, y, z, k, l)

      val (g, (x, y, z, k, l)) = mkpath.run(Graph()).value

      assert(g.incident(Src, x) == Set(k))
      assert(g.incident(Tgt, x) == Set())
      assert(g.incident(Src, y) == Set(l))
      assert(g.incident(Tgt, y) == Set(k))
    }

    test("removing parts") {
      val makeAndRemove = for {
        x <- addVertex[Graph]()
        y <- addVertex()
        z <- addVertex()
        k <- addEdge(x, y)
        l <- addEdge(y, z)
        _ <- remPart(y)
      } yield (x, y, z, k, l)

      val (g, (x, y, z, k, l)) = makeAndRemove.run(Graph()).value

      assert(g.parts(V) contains x)
      assert(g.parts(V) contains z)
      assert(!(g.parts(V) contains y))
      assert(g.parts(E).isEmpty)
    }
  }

}
