package semagrams.acsets.simple

import semagrams._
import acsets._
import acsets.abstr._
import semagrams.util._

import semagrams.widgets._

import upickle.default._
import scala.util.Random





/* Traits */
sealed trait SchemaElt extends Elt
// sealed trait Generator[+E<:Elt] extends Atomic[E]

sealed trait SchemaOb extends SchemaElt 
  with Ob
sealed trait SchemaArrow extends SchemaElt
  with Property

/* Object generators */

case class Table(override val name:String) 
  extends SchemaOb with GenOb
  derives ReadWriter
  

type TableDef = String | Ob | Option[String] | Option[Ob]
object Table:
  def apply() = new Table(makeId("Table"))
  def apply(defn:TableDef): Table = defn match
    case s:String => Table(s)
    case t:Table => t
    case ob:Ob => Table(ob.label)
    case Some(s:String) => Table(s)
    case Some(ob:Ob) => Table(ob)
    case None => Table() 
  
object Tables:
  def apply(defs:TableDef*): Seq[Table] = defs.map(Table.apply)

case class ValType[T:ReadWriter](override val name:String) 
  extends SchemaOb with GenType[T]:
  // def read(s:String) = fromStr[T](s)
  // def write(t:T): String = toStr[T](t)

  type ValueType = T
  def rw = readwriter[T]

/* Arrow generators */
case class FKey(override val name:String,dom:Table,codom:Table) 
  extends GenHom[Table]

type FKeyDef = (String, TableDef, TableDef) | (TableDef, TableDef)
object FKey:

  def apply(defn:FKeyDef): FKey = defn match
    case (nm,src,tgt) => FKey(nm,Table(src),Table(tgt))
    case (src,tgt) => FKey(makeId("FKey"),Table(src),Table(tgt))
object FKeys:
  def apply(defs:FKeyDef*) = defs.map(FKey.apply)

  // override type DomType = Table
  // override type CodomType = Table

  // type Value = Part
    
  //   pt => (pt.ob,pt.id),
  //   pair => Part(pair._1,pair._2)
  // )

// sealed trait ColumnX extends AbstractAttr[Table,AType]


case class Column[T:ReadWriter](override val name:String,dom:Table,codom:ValType[T]) 
  extends GenAttr[Table,T] with GenArrow[Table]
  // extends GenAttr[Table,ValType,T]
  // with ColumnX
  // :
  // def generators = dom.generators ++ codom.generators :+ this

  // type Value = T
  // val rw = readwriter[T]

// case class Column[T:ReadWriter](name:String,dom:Table,codom:ValType[T]) 
//   extends GenAttr[Table,T,ValType[T]]
  // override type DomType = Table
  // override type CodomType = ValType[T]






/* Constructors */


/* Objects */

// placeholder
case class Query() extends SchemaOb:
  def generators = Seq()
  def label = "query"



// placeholder
case class ProdType() 
  extends SchemaOb with TypeOb:
  // type ScalaType = Unit
  def generators = Seq()
  def label = "prod"
  type ValueType = Unit
  val rw = readwriter[Unit]
  // def read(s:String) = Some(())
  // def write(ts: ScalaType): String = label


/* Arrows */






/* Schemas */


case class SimpleSchema(
  obs:Seq[Table] = Seq(),
  homs:Seq[FKey] = Seq(),
  attrs:Seq[Column[_]] = Seq(),
  globalProps: Seq[Property] = Seq()
):

  def toACSet: SimpleACSet[SimpleSchema] =
    SimpleACSet(
      schSchema,
      PropMap(),
      PartStore()
    )(simpleSchemaIsSchema)

val schTables = Tables("Table","ValType","FKey","Column")
val Seq(tableOb,valtypeOb,fkeyOb,columnOb) = schTables

val schFKeys = FKeys(
  ("fkeySrc","FKey","Table"),
  ("fkeyTgt","FKey","Table"),
  ("attrSrc","Attr","Table"),
  ("attrTgt","Attr","ValType")
)
val Seq(fkeySrc,fkeyTgt,attrSrc,attrTgt) = schFKeys
val schSchema = SimpleSchema(
  schTables,
  schFKeys
)





val simpleSchemaIsSchema: DynSchema[SimpleSchema] = 
  new DynSchema[SimpleSchema]  {

    val name = "simpleSchemaIsSchema"
    type SchOb = Table

    // type SchTable = Table
    type SchHom = FKey
    type SchAttr = Column[_]

    // type SchType =  ValType[_]
    // type SchemaElt = Table | FKey | Column[_]


    extension (s:SimpleSchema)
      def obs = s.obs
      def homs = s.homs
      def attrs = s.attrs
      def addElts(elts:Seq[Generator]) = s.copy(
        obs = s.obs ++ elts.collect{ case x:Table => x},
        homs = s.homs ++ elts.collect{ case f:FKey => f},
        attrs = s.attrs ++ elts.collect{ case a:Column[_] => a}
      )

      def addProps(newProps:Seq[Property]) =
        
        s.copy(
          globalProps = (s.globalProps ++ newProps).distinct
        )



      // def generators = obs ++ homs ++ attrs
  
}
object SimpleSchema:
  // val tps = SimpleACSetTypes
  def apply(elts:(Elt | Property)*) = new SimpleSchema(
    elts.collect{ case x:Table          => x },
    elts.collect{ case f:FKey           => f },
    elts.collect{ case a:Column[_]  => a },
    elts.collect{ case p:Property       => p }
  )


/* Parts */



// case class Part(ob:Table,id:Id) 
//   // extends Part 
//   derives ReadWriter {
//   val ty = ob

//   def idNum = id.id

//   /** Transform to an name that is usable in tikz */
//   def tikzName: String = ob.label + idNum.toString
// }

// object Part:
//   def apply(ob:Table,i:Int) = new Part(ob,Id(i))


type PartStore = Map[Ob,PartSet]

object PartStore:
  def apply() = Map[Ob,PartSet]().withDefaultValue(PartSet())

  


case class SimpleACSet[+S:Schema](
  schema:S,
  globalData: PropMap,
  partStore: PartStore
)

object SimpleACSet:
  def apply(): SimpleACSet[SimpleSchema] = 
    implicit val schDef: Schema[SimpleSchema] = simpleSchemaIsSchema
    SimpleACSet(SimpleSchema())
  def apply[S:Schema](sch:S): SimpleACSet[S] = 
    SimpleACSet(sch,PropMap())
  def apply[S:Schema](sch:S,props:PropMap): SimpleACSet[S] = 
    SimpleACSet(sch,props,PartStore())


  def simpleACSetIsACSet[S](
    using sis:Schema[S],
    // did:PartData[D]
  ):ACSetWithSchAndData2[S,PropMap][SimpleACSet[S]] =
    new ACSet[SimpleACSet[S]] {

      val name = "SimpleACSet - " + sis.name

  
      type Data = PropMap
      implicit val dataIsPartData: PartData[PropMap] =
        PartData.propsAreData
      // implicit val dataIsPartData: PartData[Data] = 
        

      type Sch = S
      implicit val schemaIsSchema: Schema[Sch] = 
        sis
      def fromSchema(s: Sch, props: PropMap): SimpleACSet[S] = 
        SimpleACSet(s,props)
    
      extension (a:SimpleACSet[S])
        def schema = a.schema
        def globalData = a.globalData
        def setGlobalProp(f:Property,v:f.Value) = a.copy(
          globalData = a.globalData + (f,v)
        )
        def remGlobalProp(f:Property) = a.copy(
          globalData = a.globalData - f
        )

        def _getParts(ob:ACSetOb): Seq[Part] = 
          a.partStore(ob).ids.map(Part(ob,_))

        def getData(p:Part): PropMap = 
          a.partStore.get(p.ob).flatMap(
            _.propStore.get(p.id)
          ).getOrElse(PropMap())

        def _addParts(ob:ACSetOb,data:Seq[PropMap]): (SimpleACSet[S],Seq[Part]) = 
          val (nextParts,newIds) = a.partStore(ob).addParts(data) 
          val nextACSet = a.copy(
            partStore = a.partStore + (ob -> nextParts)
          )
          (nextACSet,newIds.map(Part(ob,_)))
          
          
        def remParts(ps:Seq[Part]) = a.copy(
          partStore = a.partStore ++ 
            ps.groupBy(_.ob).map( (ob,someps) =>
              ob -> a.partStore(ob).remParts(someps.map(_.id))              
            )
        )

        def moveToIndex(p: Part, idx: Int) = a.copy(
          partStore = a.partStore + (
            p.ob -> a.partStore(p.ob).moveToIndex(p.id,idx)
          )
        )
        
        def setData(kvs:Seq[(Part,PropMap)]) = a.copy(
          partStore = a.partStore ++ 
            kvs.groupBy(_._1.ob).map( (ob,somekvs) =>
              ob -> a.partStore(ob).setData(
                somekvs.map( (part,props) => (part.id,props))
              )              
            )
        )


      // extension (a:SimpleACSet[S])

      //   def _addParts(ob: ACSetOb, data: Seq[Data]): (SimpleACSet[S],Seq[Part]) = 
      //     ???
      //   def _getParts(ob: ACSetOb): Seq[Part] = 
      //     ???
      //   def getData(part: Part): Data = 
      //     ???
      //   def globalData: Data = 
      //     ???
      //   def moveToIndex(p: Part, i: Int): SimpleACSet[S] = 
      //     ???
      //   def remGlobalProp(f: Property): SimpleACSet[S] = 
      //     ???
      //   def remParts(ps: Seq[Part]): SimpleACSet[S] = 
      //     ???
        
      //   def schema: Sch = 
      //     ???
      //   def setData(kvs: Seq[(Part, Data)]): SimpleACSet[S] = 
      //     ???
      //   def setGlobalProp(f: Property, v: f.Value): SimpleACSet[S] = 
      //     ???

}
//   // val schDef = summon[Schema[S]]
//   // _simpleACSetIsACSet[schDef.SchOb,S](sch)(schDef)
  
// // def _simpleACSetIsACSet[Ob0<:Ob,S:SchemaWithOb[Ob0]](sch:S): ACSetWithObAndData[Ob0,PropMap][SimpleACSet[S]] = 
//   new ACSet[SimpleACSet[S]] {
//   type Data = PropMap
//   implicit val dataIsPartData: PartData[PropMap] = PartData.propsAreData

//   // type ACSetOb = Ob0
//   type Sch = S
//   implicit val schemaIsSchema: Schema[SimpleSchema] = simpleSchemaIsSchema
//   // implicit val 
//   // val schema = sch
//   type ACSetOb = schemaIsSchema.SchOb

//   def fromSchema(s:S,props:PropMap) = SimpleACSet(s,props)

  
//   // val siss  = summon[Schema[S]] 
//   // val schemaIsSchema: SchemaWithOb[Ob0][S] = siss

// }
// object SimpleACSet:
//   def apply[S:Schema](schema:S) = new SimpleACSet(
//     schema,
//     PropMap(),
//     PartStore()
//   )
  // def partData(props:PropMap) = props
  // def getDataProps(data:PropMap) = data

  // def setDataProp(f:Property,data:PropMap,v:f.Value) =
  //   data + (f -> v)
  // def remDataProp(f:Property,data:PropMap) = data - f


  // def setProp(data:PropMap,f:Property,v:f.Value) =
  //   data.set(f,v)
  // def remProp(data:PropMap,f:Property) =
  //   data - f
  // def partsData(ptype:Table): Map[Part,PropMap] =
  //   partStore(ptype).propStore.map((id,props) =>
  //     Part(ptype,id) -> props  
  //   )

  
  

  // def tryData(part:Part) =
  //   partStore(part.ob).tryProps(part.id)

  

  // def setData(part:Part,data:PropMap) = 
  //   this.copy(
  //     partStore = partStore.updated(part.ob,
  //       partStore(part.ob).setProps(part.id,data)
  //     )
  // )

  // def hasPart(part: Part): Boolean =
  //   partStore(part.ob).contains(part.id)

  // def addParts(ob: Table, props: Seq[PropMap]): (SimpleACSet, Seq[Part]) = 
  //   val (newparts,ids) = partStore(ob).addParts(props)
  //   (
  //     this.copy(partStore = partStore + (ob -> newparts)),
  //     ids.map(Part(ob,_))
  //   )

  // def remPart(part:Part): SimpleACSet =
  //   this.copy(partStore = partStore + 
  //     (part.ob -> partStore(part.ob).remPart(part.id))
  //   )
  
  // def hasSubpart(f:Property, part:Part): Boolean = 
  //   hasPart(part) & getData(part).contains(f)
  // def setProp(f:Property, part:PartType, v:f.Value): SimpleACSet =
  //   this.copy(partStore = partStore +
  //     (part.ob -> partStore(part.ob).setProp(part.id,f,v))
  //   )
  // def remSubpart(f:Property, part:PartType): SimpleACSet =
  //   this.copy(partStore = partStore + 
  //     (part.ob -> partStore(part.ob).remProp(f,part.id))
  //   )
  

  





// extension (pstore:PartStore)

  // def obProps(ob:Table): Map[Part,PropMap] =
  //   pstore(ob).propStore.map((id,props) =>
  //     Part(ob,id) -> props  
  //   )

  // def partProps(p:Part): PropMap =
  //   pstore(p.ob).propStore(p.id)

  // def setPartProps(p:Part,props:PropMap) =
  //   pstore + (p.ob ->
  //     pstore(p.ob).copy(
  //       propStore = pstore(p.ob).propStore + (p.id -> props)
  //     )
  //   )

  // def addPartProps

  // def tryObProp(ob:Table,f:Property): Map[Part,Option[f.Value]] =
  //   obProps(ob).map((pt,props) => (pt,props.get(f)))

  // def getobProp(ob:Table,f:Property): Map[Part,f.Value] =
  //   tryObProp(ob,f).map((part,opt) => (part,opt.get))

  // def tryPartProp(p:Part,f:Property): Option[f.Value] =
  //   partProps(p).get(f)

  // def getPartProp(p:Part,f:Property): f.Value =
  //   tryPartProp(p,f).get

  // def contains(p:Part) =
  //   pstore.contains(p.ob)
  //     & pstore(p.ob).contains(p.id)

//   def contains(p:Part,f:Property): Boolean =
//     contains(p) & partProps(p).contains(f)
