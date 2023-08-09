package semagrams.deltalens

trait Comonoid {
  type Object
  type Morphism

  def checkvalidity(a: Object): Boolean

  def checkvalidity(dom: Object, f: Morphism): Boolean

  def codom(dom: Object, f: Morphism): Object

  def compose(a: Object, f: Morphism, g: Morphism): Morphism

  def id(a: Object): Morphism
}

class CodomError(
  val c: Comonoid,
  val dom: c.Object,
  val f: c.Morphism
) extends Exception

class ComposeError(
  val c: Comonoid,
  val a: c.Object,
  val f: c.Morphism,
  val g: c.Morphism
) extends Exception

trait DeltaLens {
  val dom : Comonoid
  val codom : Comonoid

  def ap(x: dom.Object): codom.Object

  def ap(a: dom.Object, f: dom.Morphism): codom.Morphism

  def lift(x: dom.Object, f: codom.Morphism): dom.Morphism
}

class ComposedLens(
  val F: DeltaLens,
  val G: DeltaLens {
    val dom : Comonoid {
      type Object = F.codom.Object;
      type Morphism = F.codom.Morphism
    }
  }
) extends DeltaLens {

  val dom : Comonoid {
    type Object = F.dom.Object;
    type Morphism = F.dom.Morphism
  } = F.dom

  val codom : Comonoid {
    type Object = G.codom.Object;
    type Morphism = G.codom.Morphism
  } = G.codom

  def ap(x: dom.Object) = G.ap(F.ap(x))

  def ap(a: dom.Object, f: dom.Morphism) = G.ap(F.ap(a), F.ap(a, f))

  def lift(x: dom.Object, f: codom.Morphism) = F.lift(x, G.lift(F.ap(x), f))
}

class IdLens(val c: Comonoid) extends DeltaLens {

  val dom : Comonoid {
    type Object = c.Object;
    type Morphism = c.Morphism
  } = c

  val codom : Comonoid {
    type Object = c.Object;
    type Morphism = c.Morphism
  } = c

  def ap(x: dom.Object) = x

  def ap(a: dom.Object, f: dom.Morphism) = f

  def lift(x: dom.Object, f: codom.Morphism) = f
}

trait KeyedDeltaLens {
  val dom : Comonoid
  val codom : Comonoid

  type Key

  def keys(x: dom.Object): Set[Key]

  def ap(k: Key, x: dom.Object): Option[codom.Object]

  def ap(k: Key, x: dom.Object, f: dom.Morphism): Option[codom.Morphism]

  def lift(k: Key, x: dom.Object, f: codom.Morphism): Option[dom.Morphism]
}

trait EndoDeltaLensPoly {
  val interface : Comonoid

  val structure : Comonoid

  val out : DeltaLens {
    val dom : Comonoid {
      type Object = structure.Object
      type Morphism = structure.Morphism
    }
    val codom : Comonoid {
      type Object = interface.Object
      type Morphism = interface.Morphism
    }
  }

  val in : KeyedDeltaLens {
    val dom : Comonoid {
      type Object = structure.Object
      type Morphism = structure.Morphism
    }
    val codom : Comonoid {
      type Object = interface.Object
      type Morphism = interface.Morphism
    }
  }
}

class Nested(val p: EndoDeltaLensPoly) extends Comonoid {
  enum Object {
    case Empty(interface: p.interface.Object)
    case Node(structure: p.structure.Object, children: Map[p.in.Key, Object])
  }

  def interfaceof(x: Object) = x match {
    case Object.Empty(interface) => interface
    case Object.Node(structure, _children) => p.out.ap(structure)
  }

  def checkvalidity(a: Object): Boolean = a match {
    case Object.Empty(interface) => p.interface.checkvalidity(interface)
    case Object.Node(structure, children) => {
      p.structure.checkvalidity(structure)
        && p.in.keys(structure) == children.keySet
        && children.map(
          {
            case (k, ob) => {
              p.in.ap(k, structure) == interfaceof(ob)
                && checkvalidity(ob)
            }
          }
        ).forall(identity)
    }
  }

  enum Morphism {
    case Empty(interface: p.interface.Morphism)
    case Node(structure: p.structure.Morphism, children: Map[p.in.Key, (p.in.Key, Morphism)])
  }

  def checkvalidity(dom: Object, f: Morphism): Boolean = (dom, f) match {
    case (Object.Empty(interfaceObject), Morphism.Empty(interfaceMorphism)) =>
      p.interface.checkvalidity(interfaceObject, interfaceMorphism)
    case (Object.Node(structureObject, childrenObjects), Morphism.Node(structureMorphism, childrenMorphisms)) => {
      val structureCodom = p.structure.codom(structureObject, structureMorphism)
      val structureCodomKeys = p.in.keys(structureCodom)
      p.structure.checkvalidity(structureObject, structureMorphism)
      && childrenMorphisms.forall(
        { case (kNew, (kOrig, fNested)) => {
           structureCodomKeys.contains(kNew)
           && childrenObjects
             .get(kOrig)
             .map(nestedObject => checkvalidity(nestedObject, fNested))
             .getOrElse(false)
         }
        }
      )
    }
    case (_, _) => false
  }

  def codom(dom: Object, f: Morphism): Object = (dom, f) match {
    case (Object.Empty(interfaceObject), Morphism.Empty(interfaceMorphism)) =>
      Object.Empty(p.interface.codom(interfaceObject, interfaceMorphism))
    case (Object.Node(structureObject, childrenObjects), Morphism.Node(structureMorphism, childrenMorphisms)) => {
      val structureCodom = p.structure.codom(structureObject, structureMorphism)
      val structureCodomKeys = p.in.keys(structureCodom)
      Object.Node(
        structureCodom,
        structureCodomKeys
          .map(kNew =>
            {
              kNew -> (childrenMorphisms.get(kNew) match {
                case Some((kOrig, fNested)) => codom(childrenObjects(kOrig), fNested)
                case None => Object.Empty(p.in.ap(kNew, structureCodom).get)
              })
            }
          ).toMap
      )
    }
    case (_, _) => throw new CodomError(this, dom, f)
  }

  def compose(a: Object, f: Morphism, g: Morphism): Morphism = (a, f, g) match {
    case (Object.Empty(aInterface), Morphism.Empty(fInterface), Morphism.Empty(gInterface)) =>
      Morphism.Empty(p.interface.compose(aInterface, fInterface, gInterface))
    case (
      Object.Node(aStructure, aChildren),
      Morphism.Node(fStructure, fChildren),
      Morphism.Node(gStructure, gChildren)
    ) => {
      val hStructure = p.structure.compose(aStructure, fStructure, gStructure)
      val hStructureCodom = p.structure.codom(aStructure, hStructure)
      val hStructureCodomKeys = p.in.keys(hStructureCodom)
      Morphism.Node(
        hStructure,
        hStructureCodomKeys
          .map(kC =>
            {
              kC -> (
                gChildren.get(kC).flatMap {
                  case (kB, gNested) => {
                    fChildren.get(kB).map {
                      case (kA, fNested) => {
                        (kA, compose(aChildren(kA), fNested, gNested))
                      }
                    }
                  }
                }
              )
            }
          )
          .collect({ case (kC, Some(x)) => (kC, x) })
          .toMap
      )
    }
    case _ => throw new ComposeError(this, a, f, g)
  }

  def id(a: Object) = a match {
    case Object.Empty(interface) => Morphism.Empty(p.interface.id(interface))
    case Object.Node(structure, children) =>
      Morphism.Node(p.structure.id(structure), children.map({ case (k, nested) => (k, (k, id(nested))) }))
  }
}
