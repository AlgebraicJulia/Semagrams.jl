package semagrams.sprites

import com.raquo.laminar.api.L.svg._
import com.raquo.laminar.api._
import semagrams.util._
import semagrams._
import semagrams.acsets._
import semagrams.layout._

extension [A](s: L.Signal[Seq[A]]) {
  def splitWithIndexAndLength[B, C](
      f: A => B
  )(g: (B, A, L.Signal[A], L.Signal[(Int, Int)]) => C): L.Signal[Seq[C]] = {
    s.map(xs => {
      val n = xs.length
      xs.zipWithIndex.map({ case (a, i) => (a, (i, n)) })
    }).split(t => f(t._1))((b, t, $t) => g(b, t._1, $t.map(_._1), $t.map(_._2)))
  }
}

/** A Sprite that is a box with ports on both sides.
  *
  * @param `boxSprite`
  *   the sprite to use for the box
  *
  * @param `inPortSprite`
  *   the sprite to use for the left hand ports
  *
  * @param `outPortSprite`
  *   the sprite to use for the right hand ports
  *
  * @param inPort
  *   the object in the subschema to query for the list of in ports
  *
  * @param outPort
  *   the object in the subschema to query for the list of out ports
  */
case class DPBox(
    boxSprite: Sprite,
    inPortSprite: Sprite,
    outPortSprite: Sprite,
    inPort: Ob,
    outPort: Ob,
    portStyle: (ACSet, Part) => PropMap
) extends Sprite {

  override def layout(bb: BoundingBox, data: ACSet): ACSet =
    val p_in = data.parts(ROOT, inPort)
    val p_out = data.parts(ROOT, outPort)

    val (c_in, c_out) = DPBox.layoutPorts(bb, p_in.length, p_out.length)

    ((p_in zip c_in) ++ (p_out zip c_out))
      .foldLeft(data) { case (data, ((part, _), ctr)) =>
        data.setSubpart(part, Center, ctr + bb.dims / 2.0)
      }

  def computePortCenters(data: ACSet): ACSet = {
    val bbox = boxSprite.bbox(ROOT, data).get
    layout(bbox, data)
  }

  def stylePorts(data: ACSet, style: (ACSet, Part) => PropMap) =
    val pts = data.parts(ROOT, inPort) ++ data.parts(ROOT, outPort)
    var mod = data
    for (pt, acs) <- pts
    yield
      val typeProps = style(mod.subacset(pt), pt)
      mod = mod.setSubpartProps(pt, typeProps)
    mod

  def present(
      ent: Entity,
      init: ACSet,
      updates: L.Signal[ACSet],
      attachHandlers: HandlerAttacher
  ): L.SvgElement = {
    val rect = boxSprite.present(ent, init, updates, attachHandlers)

    val laid_out = updates
      .map(acset => computePortCenters(acset))
      .map(acset => stylePorts(acset, portStyle))

    val inPorts = laid_out
      .map(acset => acset.parts(ROOT, inPort))
      .split(_._1)((p, d, $d) => {
        inPortSprite.present(
          ent.asInstanceOf[Part].extendPart(p),
          d._2,
          $d.map(_._2),
          attachHandlers
        )
      })

    val outPorts = laid_out
      .map(_.parts(ROOT, outPort))
      .split(_._1)((p, d, $d) => {
        outPortSprite.present(
          ent.asInstanceOf[Part].extendPart(p),
          d._2,
          $d.map(_._2),
          attachHandlers
        )
      })

    g(
      rect,
      L.children <-- inPorts,
      L.children <-- outPorts
    )
  }

  override def boundaryPt(
      subent: Entity,
      data: ACSet,
      dir: Complex
  ): Option[Complex] = subent match {
    case Part(Nil) => boxSprite.boundaryPt(subent, data, dir)
    case _         => None
  }

  override def center(subent: Entity, data: ACSet): Option[Complex] =
    subent match {
      case Part(Nil) => data.props.get(Center)
      case Part((ob, i) :: Nil) if ob == inPort || ob == outPort => {
        computePortCenters(data).trySubpart(Center, subent.asInstanceOf[Part])
      }
      case _ => None
    }

  override def bbox(subent: Entity, data: ACSet) = subent match
    case p: Part =>
      val (spr, tail) = p.ty.path match
        case Seq()                    => (boxSprite, ROOT)
        case ip if ip == Seq(inPort)  => (inPortSprite, p.tail)
        case op if op == Seq(outPort) => (outPortSprite, p.tail)
        case _ => throw msgError(s"DPBox bbox: unmatched part $subent")
      spr.bbox(tail, computePortCenters(data).subacset(p))
    case _ => throw msgError(s"DPBox bbox: unmatched entity $subent")

  override def toTikz(b: Part, data: ACSet, visible: Boolean = true): String =

    val boxString = boxSprite.toTikz(b, data)

    val inPorts = data.partsMap(inPort).ids.map(b.extend(inPort, _))
    val outPorts = data.partsMap(outPort).ids.map(b.extend(outPort, _))

    val inString = tikzInPorts(inPorts)
    val outString = tikzOutPorts(outPorts)

    boxString + inString + outString + "\n"

}

object DPBox {

  /** A pure algorithm for port layout */
  def layoutPorts(
      bb: BoundingBox,
      n_in: Int,
      n_out: Int
  ): (Seq[Complex], Seq[Complex]) = (
    0 until n_in map (i =>
      Complex(
        bb.pos.x - bb.dims.x / 2.0,
        bb.pos.y - (bb.dims.y / 2.0) + (1 + 2 * i) * (bb.dims.y / n_in) / 2.0
      )
    ),
    0 until n_out map (i =>
      Complex(
        bb.pos.x + bb.dims.x / 2.0,
        bb.pos.y - (bb.dims.y / 2.0) + (1 + 2 * i) * (bb.dims.y / n_out) / 2.0
      )
    )
  )

  /** Update `data` with centers for `inPort`s and `outPort`s */
  def layoutPorts(inPort: Ob, outPort: Ob)(
      bb: BoundingBox,
      data: ACSet
  ): ACSet =
    val p_in = data.parts(ROOT, inPort)
    val p_out = data.parts(ROOT, outPort)

    val (c_in, c_out) = layoutPorts(bb, p_in.length, p_out.length)

    ((p_in zip c_in) ++ (p_out zip c_out))
      .foldLeft(data) { case (data, ((part, _), ctr)) =>
        data.setSubpart(part, Center, ctr)
      }

  /** Update `data` with centers for `inPort`s and `outPort`s on the viewport
    * boundary
    */
  def layoutPortsBg(inPort: Ob, outPort: Ob)(sz: Complex, data: ACSet): ACSet =
    layoutPorts(inPort, outPort)(BoundingBox(sz / 2.0, sz), data)

  /** Compute the index of a new port based on a position and the number of
    * existing ports
    */
  def portNumber(pos: Complex, size: Complex, nports: Int) =
    val l = (0 to nports + 1).map(
      _ * size.y / (nports + 1)
    )
    l.lastIndexWhere(_ < pos.y)

}
