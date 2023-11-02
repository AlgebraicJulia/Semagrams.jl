package semagrams.acsets.abstr

import semagrams._
import semagrams.acsets._
import semagrams.util._


import semagrams.acsets.simple.SimpleSchema
import semagrams.acsets.simple.simpleSchemaIsSchema

import scala.language.implicitConversions

import upickle.default._



/* General categorical entities (e.g., `SchOb`, `Arrow`) */
trait Elt:
  def generators: Seq[Generator]
  def label:String
  override def toString(): String = if label != ""
    then label
    else s"AnonOb(${generators.map(_.id).mkString(",")})"



trait Ob extends Elt with EntityType

// case object UnitOb extends Ob:
//   def generators = Seq()
//   def label = "UnitOb"
// val backgroundPart = Part(UnitOb,0)


trait Arrow[+X<:Ob] extends Elt:
  def dom: X    
  def codom: Ob
  def path: Seq[GenArrow[_]]


// trait Entity extends Ob
// // type ATable = AbstractTable

trait TypeOb extends Ob
//   type ValueType
//   def rw: ReadWriter[ValueType]
// type AType = AbstractType


trait Hom[+X<:Ob] extends Arrow[X]:
  // type Value = Part
  // def rw(using yRW:ReadWriter[Y]): ReadWriter[Part] = Part.rw[Y]
  // // val dom: SchOb
  val codom: X
 
trait Attr[+X<:Ob,Y<:TypeOb] extends Arrow[X]:
//   // type Value = T
//   // val rw = readwriter[T]
//   val dom: SchOb
  val codom: Y

// type AAttr[SchOb<:AOb,Y[_]<:AType[_]] = AbstractAttr[SchOb,Y[_]]

/* Generating categorical entities */
trait Generator extends Elt:
  val id:UUID
  def generators = Seq(this)
  def name: String
  def label = name


  override def toString = if name != ""
    then name
    else id.toString


def genMap[G<:Generator](iter:Iterable[G]): Map[UUID,G] =
  iter.map(g => g.id -> g).toMap


object Generator:
  implicit def genIsPair[G<:Generator](g:G): (UUID,G) = g.id -> g

trait GenOb extends Ob with Generator

trait GenType[T:ReadWriter] extends TypeOb with GenOb
 
trait GenArrow[+X<:Ob] extends Arrow[X] with Generator with Property:
  def path = Seq(this)
  override def generators = dom.generators ++ codom.generators :+ this

trait GenHom[+X<:Ob:ReadWriter] extends GenArrow[X] 
  with Hom[X]:
  type Value = Part
  val rw: ReadWriter[Value] = Part.rw[X]

trait GenAttr[+X<:Ob,T:ReadWriter] extends GenArrow[X]
  with Attr[X,GenType[T]]:
  type Value = T
  val rw = readwriter[T]


  
case class Part(id:UUID,ob:Ob) extends Entity:

  val ty = ob

  override def toString = if ob.label == ""
    then "Part" + id.rand
    else ob.label + id.rand


  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.label + id.toString


object Part:
  def apply(x:Ob) = new Part(UUID("Part"),x)
  def apply(id:UUID,x:Ob) = new Part(id,x)
  def rw[X<:Ob:ReadWriter]: ReadWriter[Part] = 
    readwriter[(String,UUID)].bimap[Part](
      part => (part.ob match
        case ob:X => (write(ob),part.id)
        case ob => throw util.msgError(s"Bad ob read $ob")
        ),
      (obstr,id) => Part(id,read[X](obstr))
    )


type SchemaWithOb[X] = [S] =>> Schema[S] { type SchOb >: X }
trait Schema[S]:
  type SchOb <: GenOb
  type SchType[T] <: GenType[T]
  type SchHom <: GenHom[SchOb]
  type SchAttr <: GenAttr[SchOb,_]

  // type SchTable <: AOb & GenElt
  // type SchType <: AType[_] & GenElt
  // type SchHom <: GenHom[SchTable,SchTable]
  // type SchAttr <: [T] =>> GenAttr[SchTable,SchType]
  
  // type Part = Part
  val name:String
  override def toString = name

  val emptySchema: S

  extension (s:S)

    def obMap: Map[UUID,SchOb]
    def homMap: Map[UUID,SchHom]
    def attrMap: Map[UUID,SchAttr]
    def globalProps: Seq[Property] = Seq()


    def _addElts(gens:Seq[Generator]): S
    def addProps(prop:Seq[Property]): S

    def _remElts(gens:Seq[Generator]): S
    def remProps(props:Seq[Property]): S


    def eltMap: Map[UUID,Elt] = obMap ++ homMap ++ attrMap

    def obs: Seq[SchOb] = obMap.values.toSeq
    def homs: Seq[SchHom] = homMap.values.toSeq
    def attrs: Seq[SchAttr] = attrMap.values.toSeq
    def elts: Seq[Elt] = eltMap.values.toSeq


    def display = "Schema(" + 
      (if s.obs.isEmpty then "" else 
        "\n  SchOb:   " + s.obs.mkString(", ")
      ) + (if s.homs.isEmpty then "" else 
        "\n  Hom:  " + s.homs.mkString(", ")
      ) + (if s.attrs.isEmpty then "" else 
        "\n  Observable: " + s.attrs.mkString(", ")
      ) + (if s.globalProps.isEmpty then "" else 
        "\n  Prop: " + s.globalProps.mkString(", ")
      ) + (if (s.generators ++ s.globalProps).isEmpty 
        then ")\n" else "\n)\n"
      )


    def generators: Seq[Generator] = (obs ++ homs ++ attrs).flatMap(_.generators)

    def contains(ob:Ob): Boolean = obs.contains(ob)
    def tryOb(id0:UUID): Option[SchOb] = obs.find(_.id == id0)
    def getOb(id0:UUID): SchOb = tryOb(id0).get

    
    def addElts(elts:Seq[Elt]): S = 
      _addElts(elts.flatMap(_.generators))

    def addElt(elt:Elt): S = 
      _addElts(elt.generators)
    def addProp(prop:Property): S = 
      addProps(Seq(prop))


    def remElt(elt:Elt) = s._remElts(elt.generators)
    def remElts(elts:Seq[Elt]) = s._remElts(elts.flatMap(_.generators))



    def +(elt:Elt) = s.addElt(elt)
    def +(prop:Property) = s.addProp(prop)
    def ++(elts:Seq[Elt]) = s.addElts(elts)
    def ++[SS:Schema](other:SS) = 
      s.addElts(other.generators)

    def --(elts:Seq[Elt]): Seq[Generator] = generators.diff(elts)
    def --(that:S): Seq[Generator] = --(that.generators)


    // def ++(stuff:Seq[AElt | Property]): S = addElts(
    //   stuff.collect{ case x:AElt => x}
    //     .flatMap(_.generators)
    //   ).addProps(stuff.collect{ case p:Property => p})

    // def ++(s:S): S = ++(s.generators) ++ s.props


object Schema:
  def apply[S:Schema](): S = summon[Schema[S]].emptySchema
  def apply(): SimpleSchema = apply[SimpleSchema]()(simpleSchemaIsSchema)

trait PartData[Data]:
  def toData(props:PropMap): Data
  def newData(): Data = toData(PropMap()) 
  extension (d:Data)
    /* Implementation API */
    def getProps(): PropMap
    def setProp(f:Property,v:f.Value): Data
    def remProp(f:Property): Data
    def merge(that:Data): Data
    
    /* Generic methods */
    def hasProp(f:Property): Boolean = d.getProps().contains(f)
    def tryProp(f:Property): Option[f.Value] = d.getProps().get(f)
    def getProp(f:Property): f.Value = d.getProps()(f)
    
    def hasProps(fs:Seq[Property]): Boolean = d.getProps().contains(fs)
    def getProps(fs:Iterator[Property]): PropMap = d.getProps().filterKeys(fs.contains)
    
    def setProps(props:PropMap): Data =
      props.pmap.keys.toSeq match
        case Seq() => d
        case f +: rest => d.setProp(f,props(f)).setProps(props - f)

    def softSetProp(f:Property,v:f.Value): Data =
      if d.hasProp(f) then d else d.setProp(f,v)
    def softSetProps(props:PropMap): Data =
      d.setProps(
        props.filterKeys(!hasProp(_))
      )
        
      


object PartData:

  def apply[D:PartData](props:PropMap = PropMap()) =
    val pd = summon[PartData[D]] 
    pd.toData(props)

  implicit val propsAreData:PartData[PropMap] = new PartData[PropMap] {
    def toData(props:PropMap) = props
    extension (props:PropMap)
      def getProps() = props
      def setProp(f:Property,v:f.Value) = props + (f,v)
      def remProp(f:Property) = props - f
      def merge(that:PropMap) = props ++ that
  }


