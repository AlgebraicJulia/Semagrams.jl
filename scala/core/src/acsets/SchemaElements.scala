package semagrams.acsets

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


case class Table(id:UUID) extends Ob with Generator:
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object Table:
  def apply(id:UUID,name:String): Table = 
    Table(id).rename(name)


case class ValType[T:ReadWriter](id:UUID) extends Ob with Generator:

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.obGenerators

object ValType:
//   import scala.collection.mutable.Map
//   val valTypes: Map[String,ValType[_]] = Map()
//   register[String]("String")
//   register[Int](UUID("ValType"),"Int")

//   // def register(tp:ValType[_]): ValType[_] = 
//   //   valTypes += (tp.id -> tp)
//   //   tp

//   def register[T:ReadWriter](id:UUID,name:String): Unit =
//     valTypes += (id -> tp)

//   def apply(id:UUID): ValType[_] = if valTypes.contains(id)
//     then valTypes(id)
//     else throw msgError(s"Error: Missing type id $id. Try providing a type at the call site.")

//   def apply(name:UUID): ValType[_] = if valTypes.contains(id)
//     then valTypes(id)
//     else throw msgError(s"Error: Missing type id $id. Try providing a type at the call site.")

  def apply[T:ReadWriter](id:UUID,name:String): ValType[T] =
    new ValType[T](id).rename(name)
  
  def apply[T:ReadWriter](name:String): ValType[T] =
    new ValType[T](UUID("ValType")).rename(name)
  

  // def apply[T:ReadWriter](): ValType[T] = valTypes.get(name) match
  //   case Some(tp) if tp == ValType[T]() => ValType[T]().rename(tp.name)
  //   case Some(tp) =>
  
    
  //   if valTypes.contains(name) & valTypes(name) == ValType[T]()
  //   then valType[T]()
  //   then valTypes
    
  //   valTypes.get(id) match
  //   case Some(tp) => tp
  //   case None =>
  //     val tp = ValType[T](id).rename(name)
  //     valTypes += (id -> tp)
  //     tp

  // def apply[T:ReadWriter](name:String = ""): ValType[_] =
  //   valTypes.values.find(_.name == name) match
  //     case Some(tp) => tp
  //     case None =>
  //       val id = UUID("ValType")
  //       val tp = ValType[T](id).rename(name)
  //       valTypes += (id -> tp)
  //       tp
    


trait Hom[+X<:Ob,+Y<:Ob] extends Property:

  def dom: X    
  def codom: Y
  def path: Seq[Hom[_,_] & Generator]

type PartHom = Hom[_,_] & PartProp

extension (f:Hom[_,_] & Generator)
  def path = Seq(f)

case class FKey(id:UUID,dom:Ob,codom:Ob) extends Hom[Ob,Ob] with Generator:
  type Value = Part
  implicit val rw = Part.rw
  
  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object FKey:
  def apply(id:UUID,name:String,dom:Ob,codom:Ob): FKey = 
    FKey(id,dom,codom).rename(name)

case class Attr[T:ReadWriter](id:UUID,dom:Ob,codom:ValType[T]) extends Hom[Ob,ValType[T]] 
  with Generator:
  override type Value = T
  override val rw = readwriter[T]

  def rename(newName:String) = Generator.rename(this,newName)
  def generators = this.homGenerators
  def path = this.path
  
object Attr:
  def apply[T:ReadWriter](id:UUID,name:String,dom:Ob,tp:ValType[T]): Attr[T] = 
    Attr[T](id,dom,tp).rename(name)


