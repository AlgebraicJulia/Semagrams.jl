package semagrams.flatacsets

import semagrams._
import scala.collection.mutable

class MutableACSet[S: IsSchema](
    val schema: S,
    var counter: Int,
    var globals: PropMap,
    val parts: Map[Ob, mutable.Seq[Part]],
    val props: mutable.Map[Part, PropMap]
) {
  def addPart(ob: Ob, pm: PropMap): Part = {
    val x = Part(counter, ob)
    parts(ob) :+ x
    counter = counter + 1
    props.put(x, pm)
    x
  }

  def addPart(ob: Ob): Part = addPart(ob, PropMap())

  def setProp(f: Property, x: Part, y: f.Value): Unit =
    props.put(x, props(x).set(f, y))

  def setSubparts(pm: PropMap, x: Part): Unit =
    props.put(x, props(x) ++ pm)

  def remSubpart(f: Property, x: Part): Unit =
    props.put(x, props(x) - f)

  def subpart(f: Property, x: Part): f.Value = props(x)(f)

  def trySubpart(f: Property, x: Part): Option[f.Value] = props(x).get(f)

  def incident(f: Property, dom: Ob, y: f.Value): Seq[Part] =
    parts(dom).filter(trySubpart(f, _) == Some(y)).toSeq

  def incident(f: HomWithDom, y: Part): Seq[Part] = incident(f, f.dom, y)

  def incident(f: AttrWithDom, y: f.Value): Seq[Part] = incident(f, f.dom, y)

  def remPart(x: Part): Unit = {
    val visited = mutable.HashSet[Part]()
    val next = mutable.Stack[Part](x)
    while (!next.isEmpty) {
      val y = next.pop()
      visited.add(y)
      for (dom <- schema.obs) {
        for (f <- schema.homs(dom).filter(_.codom == y.ob)) {
          next.pushAll(incident(f, dom, y).filter(e => !(visited contains e)))
        }
      }
    }

    parts.foreach((_, ents) => ents.filter(e => !(visited contains e)))
    props --= visited
  }

  def freeze: ACSet[S] = {
    new ACSet[S](
      schema,
      counter,
      globals,
      parts.map((ob, ents) => (ob, ents.toSeq)).toMap,
      props.toMap
    )
  }
}

object MutableACSet {
  def apply[S: IsSchema](s: S) = new MutableACSet(
    s,
    0,
    PropMap(),
    s.obs.map(ob => (ob, mutable.Seq[Part]())).toMap,
    mutable.Map[Part, PropMap]()
  )
}
