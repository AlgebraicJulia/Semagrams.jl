package semagrams.acsets.abstr

import semagrams.acsets._

// package semagrams.acsets2.abstr

import semagrams._

import upickle.default._
// import scala.annotation.targetName
// import javax.xml.validation.Schema



/* General categorical entities (e.g., `SchOb`, `Arrow`) */
trait Elt:
  def generators: Seq[Generator]
  def label:String
  override def toString(): String = label



trait Ob extends Elt with EntityType

case object UnitOb extends Ob:
  def generators = Seq()
  def label = "UnitOb"
val backgroundPart = Part(UnitOb,0)


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
trait Generator:
  def generators = Seq(this)
  def name: String// = toString
  def label = name

trait GenOb extends Ob with Generator
trait GenType[T:ReadWriter] extends TypeOb with Generator
 
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


  
case class Part(ob:Ob,id:Id) 
  extends Entity:

  def idNum = id.id
  val ty = ob


  /** Transform to an name that is usable in tikz */
  def tikzName: String = ob.label + idNum.toString


object Part:
  def apply(x:Ob,idNum:Int) = new Part(x,Id(idNum))
  def rw[X<:Ob:ReadWriter]: ReadWriter[Part] = 
    readwriter[(String,Int)].bimap[Part](
      part => (part.ob match
        case ob:X => (write(ob),part.id.id)
        case ob => throw util.msgError(s"Bad ob read $ob")
        ),
      (obstr,i) => Part(read[X](obstr),Id(i))
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

  extension (s:S)

    def obs: Seq[SchOb]
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

    def homs: Seq[SchHom]
    def attrs: Seq[SchAttr]
    def globalProps: Seq[Property] = Seq()

    def generators: Seq[Generator] = (obs ++ homs ++ attrs).flatMap(_.generators)


type DynSchemaWithOb[X<:Ob] = [S] =>> DynSchema[S] & SchemaWithOb[X][S]
trait DynSchema[S] extends Schema[S]:

  extension (s:S)
    def addElts(elts:Seq[Generator]): S
    def addProps(prop:Seq[Property]): S

    def +(elt:Elt) = s.addElts(elt.generators)
    def +(prop:Property) = s.addProps(Seq(prop))


    // def ++(stuff:Seq[AElt | Property]): S = addElts(
    //   stuff.collect{ case x:AElt => x}
    //     .flatMap(_.generators)
    //   ).addProps(stuff.collect{ case p:Property => p})

    // def ++(s:S): S = ++(s.generators) ++ s.props

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


