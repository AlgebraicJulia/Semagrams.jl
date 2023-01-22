package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets._
import semagrams.layout._

extension[A] (s: L.Signal[Seq[A]]) {
  def splitWithIndexAndLength[B,C]
    (f: A => B)
    (g: (B, A, L.Signal[A], L.Signal[(Int,Int)]) => C): L.Signal[Seq[C]] = {
    s.map(xs => {
            val n = xs.length
            xs.zipWithIndex.map({ case (a, i) => (a, (i, n)) })
    }).split(t => f(t._1))(
      (b, t, $t) => g(b, t._1, $t.map(_._1), $t.map(_._2))
    )
  }
}

/**
 * Directed Port Box: a box with ports on both sides
 */
case class DPBox(
  boxSprite: Sprite,
  inPortSprite: Sprite,
  outPortSprite: Sprite,
  inPort: Ob,
  outPort: Ob
) extends Sprite {

  def present(
    ent: Entity,
    init: ACSet,
    updates: L.Signal[ACSet],
    attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val rect = boxSprite.present(ent, init, updates, attachHandlers)

    val $bbox = updates.map(acs => boxSprite.bbox(ROOT, acs).get)

    val spacer = FixedRangeExceptEnds(-1, 1)

    val inPorts = updates.map(_.parts(ROOT, inPort))
      .splitWithIndexAndLength(_._1)(
        (p, d, $d, $in) => {
          val $c = $bbox.combineWith($in).map(
            {
              case (bbox, i, n) =>
                bbox.pos
                + Complex(0, bbox.dims.y / 2)
                + Complex(0, bbox.dims.y / 2 * spacer.assignPos(i, n))
            })
          inPortSprite.present(
            ent.extend(p),
            d._2,
            $d.combineWith($c).map({ case (_, acs, c) => acs.setSubpart(ROOT, Center, c) }),
            attachHandlers
          )
        }
      )

    val outPorts = updates.map(_.parts(ROOT, outPort))
      .splitWithIndexAndLength(_._1)(
        (p, d, $d, $in) => {
          val $c = $bbox.combineWith($in).map(
            {
              case (bbox, i, n) =>
                bbox.pos
                + Complex(bbox.dims.x, bbox.dims.y / 2)
                + Complex(0, bbox.dims.y / 2 * spacer.assignPos(i, n))
            })
          outPortSprite.present(
            ent.extend(p),
            d._2,
            $d.combineWith($c).map({ case (_, acs, c) => acs.setSubpart(ROOT, Center, c) }),
            attachHandlers
          )
        }
      )

    g(
      rect,
      L.children <-- inPorts,
      L.children <-- outPorts
    )
  }
}
