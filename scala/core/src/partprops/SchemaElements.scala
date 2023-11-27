package semagrams

import upickle.default._


import semagrams._
import semagrams.util._


/* General categorical entities (e.g., `Table`, `Arrow`) */
trait Elt:
  def id: UID
  def generators: Map[UID,Generator]
  def label:String
  override def toString(): String = if label != ""
    then label
    else s"Elt(${this.id})"

  def uses(ids:Iterable[UID]) = ids.exists(generators.contains)


extension [E<:Elt](elts:Iterable[E])
  def eltMap: Map[UID,E] = elts.map(elt => elt.id -> elt).toMap

/* Generating categorical entities */
trait Generator extends Elt:

  val id:UID

  var name: String = ""
  def label = name
  override def toString = if name != ""
    then name
    else id.toString

object Generator:
  def rename[G<:Generator](g:G,newName:String): G =
    g.name = newName
    g



trait Ob extends Elt with EntityType

extension [X<:(Ob & Generator)](x:X)
  def obGenerators = Map(x.id -> x)

extension [F<:(Hom[_,_] & Generator)](f:F)
  def homGenerators = Map(f.id -> f) ++ f.dom.generators ++ f.codom.generators


trait AbstractValType extends Ob
trait AbstractFKey extends Hom[Ob,Ob]:
  type Value = Part
  val rw = Part.rw
trait AbstractAttr extends Hom[Ob,AbstractValType]

case class Table(id:UID) extends Ob with Generator:
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object Table:
  def apply(id:UID,name:String): Table = 
    Table(id).rename(name)



case class IdHom[X<:Ob](x:X) extends AbstractFKey with Hom[X,X]:
  def id = x.id
  def dom = x
  def codom = x
  def generators = x.generators
  def path = Seq()
  def label = s"id($x)"

case class ValType[T:ReadWriter](id:UID) extends AbstractValType with Generator:

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object ValType:

  def apply[T:ReadWriter](id:UID,name:String): ValType[T] =
    new ValType[T](id).rename(name)
  
  def apply[T:ReadWriter](name:String): ValType[T] =
    new ValType[T](UID("ValType")).rename(name)
  

trait Hom[+X<:Ob,+Y<:Ob] extends Elt with Property:

  def dom: X    
  def codom: Y
  def path: Seq[Hom[_,_] & Generator]

  def display: String = toString + s": $dom -> $codom"

type PartHom = Hom[_,_] & PartProp

extension (f:Hom[_,_] & Generator)
  def path = Seq(f)

case class FKey(id:UID,dom:Ob,codom:Ob) extends AbstractFKey with Generator:
  
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object FKey:
  def apply(id:UID,name:String,dom:Ob,codom:Ob): FKey = 
    FKey(id,dom,codom).rename(name)

case class Attr[T:ReadWriter](id:UID,dom:Ob,codom:ValType[T]) extends AbstractAttr with Generator:
  override type Value = T
  override val rw = readwriter[T]

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object Attr:
  def apply[T:ReadWriter](id:UID,name:String,dom:Ob,tp:ValType[T]): Attr[T] = 
    Attr[T](id,dom,tp).rename(name)


case class Span(left:PartHom,right:PartHom)
object Span:
  def apply(left:PartHom,right:PartHom): Span =
    assert(left.dom == right.dom)
    new Span(left,right)

