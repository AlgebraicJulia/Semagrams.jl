package semagrams.acsets

import upickle.default._

import semagrams._
import semagrams.util._
import semagrams.partprops._

/* General categorical entities (e.g., `Table`, `Arrow`) */
trait Elt:
  def id: UID
  def generators: Map[UID, Generator]
  def label: String
  override def toString(): String = if label != ""
  then label
  else s"Elt(${this.id})"

  def uses(ids: Iterable[UID]) = ids.exists(generators.contains)

extension [E <: Elt](elts: Iterable[E])
  def eltMap: Map[UID, E] = elts.map(elt => elt.id -> elt).toMap

/* Generating categorical entities */
trait Generator extends Elt:

  val id: UID

  var name: String = ""
  def label = name
  override def toString = if name != ""
  then name
  else id.toString

object Generator:
  def rename[G <: Generator](g: G, newName: String): G =
    g.name = newName
    g

trait AbstractOb extends Elt:
  type PartType
  val rw: ReadWriter[PartType]

trait Ob extends AbstractOb:
  type PartType = Part
  val rw = readwriter[Part]

object Ob:
  def apply(id: UID) = new Ob with Generator { self =>
    val id: UID = id
    def generators = Map(id -> self)
  }

extension [X <: (AbstractOb & Generator)](x: X)
  def obGenerators = Map(x.id -> x)

extension [F <: (Hom[_] & Generator)](f: F)
  def homGenerators = Map(f.id -> f) ++ f.dom.generators ++ f.codom.generators

// trait AbstractValType extends Ob
// trait AbstractFKey extends PartHom
// val rw = Part.rw
// trait AbstractAttr extends Hom[Ob, AbstractValType]

// case object UnitOb extends AbstractOb {
//   override type PartType = Unit
//   override val rw = readwriter[Unit]

//   override def id: UID = UID("UnitOb")

//   override def generators: Map[UID, Generator] = Map()

//   override def label: String = "𝟙"

// }
// val unitPart = new AbstractPart {
//   val id = UID("UnitPart")
//   val ob = UnitOb
// }

case class Table(id: UID) extends Ob with Generator:

  def rename(newName: String) = Generator.rename(this, newName)
  def generators = this.obGenerators

object Table:
  def apply(id: UID, name: String): Table =
    Table(id).rename(name)

case class IdHom[T](x: Ob { type PartType = T }) extends Hom[T]:
  def id = x.id
  def dom = x
  def codom = x
  def generators = x.generators
  def path = Seq()
  def label = s"id($x)"

case class ValType[T: ReadWriter](id: UID) extends AbstractOb with Generator:
  type PartType = T
  val rw = readwriter[T]

  def rename(newName: String) = Generator.rename(this, newName)
  def generators = this.obGenerators

object ValType:

  def apply[T: ReadWriter](id: UID, name: String): ValType[T] =
    new ValType[T](id).rename(name)

  def apply[T: ReadWriter](name: String): ValType[T] =
    new ValType[T](UID("ValType")).rename(name)

trait Hom[T] extends Elt with Property:
  type Value = T
  val rw: ReadWriter[T] = codom.rw

  def dom: Ob
  def codom: AbstractOb { type PartType = T }
  def path: Seq[Hom[_] & Generator]

  def display: String = toString + s": $dom -> $codom"

trait PartHom extends Hom[Part]:
  def dom: Ob
  def codom: Ob

extension (f: Hom[_] & Generator) def path = Seq(f)

case class FKey(id: UID, dom: Ob, codom: Ob) extends PartHom with Generator:

  def rename(newName: String) = Generator.rename(this, newName)
  def generators = this.homGenerators
  def path = this.path

object FKey:
  def apply(id: UID, name: String, dom: Ob, codom: Ob): FKey =
    FKey(id, dom, codom).rename(name)

case class Attr[T: ReadWriter](id: UID, dom: Ob, codom: ValType[T])
    extends Hom[T]
    with Generator:
  override type Value = T
  override val rw = readwriter[T]

  def rename(newName: String) = Generator.rename(this, newName)
  def generators = this.homGenerators
  def path = this.path

object Attr:
  def apply[T: ReadWriter](
      id: UID,
      name: String,
      dom: Ob,
      tp: ValType[T]
  ): Attr[T] =
    Attr[T](id, dom, tp).rename(name)

case class Span[+F <: Hom[_]](left: F, right: F):
  assert(left.dom == right.dom)
  val dom = left.dom

object Span:
  def apply[F <: Hom[_]](left: F, right: F): Span[F] =
    assert(left.dom == right.dom)
    new Span(left, right)

type PartSpan = Span[PartHom]
