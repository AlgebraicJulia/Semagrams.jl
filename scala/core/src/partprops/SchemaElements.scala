package semagrams

import upickle.default._


import semagrams._
import semagrams.util._


/* General categorical entities (e.g., `Table`, `Arrow`) */
trait Elt:
  def id: UUID
  def generators: Map[UUID,Generator]
  def label:String
  override def toString(): String = if label != ""
    then label
    else s"Elt(${this.id})"

  def uses(ids:Iterable[UUID]) = ids.exists(generators.contains)


extension [E<:Elt](elts:Iterable[E])
  def eltMap: Map[UUID,E] = elts.map(elt => elt.id -> elt).toMap

/* Generating categorical entities */
trait Generator extends Elt:

  val id:UUID

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

case class Table(id:UUID) extends Ob with Generator:
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object Table:
  def apply(id:UUID,name:String): Table = 
    Table(id).rename(name)



case class ValType[T:ReadWriter](id:UUID) extends AbstractValType with Generator:

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object ValType:

  def apply[T:ReadWriter](id:UUID,name:String): ValType[T] =
    new ValType[T](id).rename(name)
  
  def apply[T:ReadWriter](name:String): ValType[T] =
    new ValType[T](UUID("ValType")).rename(name)
  

trait Hom[+X<:Ob,+Y<:Ob] extends Elt with Property:

  def dom: X    
  def codom: Y
  def path: Seq[Hom[_,_] & Generator]

type PartHom = Hom[_,_] & PartProp

extension (f:Hom[_,_] & Generator)
  def path = Seq(f)

case class FKey(id:UUID,dom:Ob,codom:Ob) extends AbstractFKey with Generator:
  
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object FKey:
  def apply(id:UUID,name:String,dom:Ob,codom:Ob): FKey = 
    FKey(id,dom,codom).rename(name)

case class Attr[T:ReadWriter](id:UUID,dom:Ob,codom:ValType[T]) extends AbstractAttr with Generator:
  override type Value = T
  override val rw = readwriter[T]

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object Attr:
  def apply[T:ReadWriter](id:UUID,name:String,dom:Ob,tp:ValType[T]): Attr[T] = 
    Attr[T](id,dom,tp).rename(name)


