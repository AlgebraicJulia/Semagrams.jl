package semagrams

import utest._
import semagrams.ACSets._
import scala.collection.immutable.HashMap

object ACSetSpec extends TestSuite {
  val GraphSchema = Schema(
    Vector("E", "V"),
    Vector("src", "tgt"),
    Vector(),
    Vector(),
    HashMap("src" -> "E", "tgt" -> "E"),
    HashMap("src" -> "V", "tgt" -> "V")
  )

  def tests = Tests {
    test("empty graph") {
      val g = ACSet[Unit](GraphSchema)
      assert(g.nparts("V") == 0)
      assert(g.nparts("E") == 0)
    }

    test("path graph") {
      val mkpath = for {
        x <- add_part("V", ())
        y <- add_part("V", ())
        z <- add_part("V", ())
        k <- add_part("E", ())
        l <- add_part("E", ())
        _ <- set_hom_subpart(k, "src", x)
        _ <- set_hom_subpart(k, "tgt", y)
        _ <- set_hom_subpart(l, "src", y)
        _ <- set_hom_subpart(l, "tgt", z)
      } yield (x,y,z,k,l)

      val (g,(x,y,z,k,l)) = mkpath.run(ACSet[Unit](GraphSchema)).value

      assert(g.nparts("V") == 3)
      assert(g.nparts("E") == 2)
      assert(g.get_parts("V").toList.contains(x))
      assert(g.get_parts("V").toList.contains(y))
      assert(g.get_parts("V").toList.contains(z))
      assert(g.has_part(x))
      assert(g.has_part(k))
      assert(g.hom_subpart(k, "src") == Some(x))
      assert(g.hom_subpart(l, "tgt") == Some(z))
    }
  }
}
