package semagrams.util

import semagrams.partprops.{Part, Center}
import semagrams.acsets.{ACSet, Ob, Hom}

def step[K](
    pos: Map[K, Complex],
    graph: Map[K, Seq[K]],
    bounds: Complex
): Map[K, Complex] =

  def corners(z0: Complex): Seq[Complex] =
    Seq[Complex](0, bounds.x, bounds.y, bounds)
      .map(c => -2 * (c - z0))

  pos.map((k, z0) =>
    k -> (
      corners(z0)
        ++ pos.map((_, z1) => 2 * (z0 - z1))
        ++ graph(k).map(k2 => pos(k2) - z0)
    ).reduce(_ + _)
  )

def forceCenters[K](
    graph: Map[K, Seq[K]],
    bounds: Complex,
    fixed: Map[K, Complex] = Map()
): Map[K, Complex] =
  import scala.util.Random
  val rand = Random()

  def helper(pos: Map[K, Complex], incr: Double): Map[K, Complex] =
    val dz = step(fixed ++ pos, graph, bounds)
    val next = pos.map((k, z0) => (k, z0 + dz(k)))

    if dz.exists((_, z) => z.abs > .1 * incr)
    then helper(next, incr)
    else next

  val inits = graph.view
    .filterKeys(k => !fixed.contains(k))
    .map((k, _) =>
      k -> Complex(
        rand.between(0.0, bounds.x),
        rand.between(0.0, bounds.y)
      )
    )
    .toMap

  helper(inits, 1)

def forceACSetCenters(
    acset: ACSet,
    vtypes: Seq[Ob],
    etypes: Seq[Hom[Part] | (Hom[Part], Hom[Part])],
    bounds: Complex
): ACSet =
  val es = etypes
    .map(_ match
      case f: Hom[Part] => acset.collectProp(f).map((p, q) => p -> Seq(q))
      case (f: Hom[Part], g: Hom[Part]) if f.dom == g.dom =>
        acset
          .getPropSeq(f.dom)
          .filter((_, props) => props.contains(f, g))
          .map((_, props) => props(f) -> props(g))
          .groupBy(_._1)
          .map((left, nbrs) => left -> nbrs.map(_._2))
      case _ => Map()
    )
    .foldLeft(Map[Part, Seq[Part]]())((aggr, add) => aggr.merge(_ ++ _)(add))

  val vs = vtypes.flatMap(acset.getParts)

  val fixed = acset.collectProp(Center, vs)

  val ctrs = forceCenters(es, bounds, fixed)

  acset.setProp(Center, ctrs)
