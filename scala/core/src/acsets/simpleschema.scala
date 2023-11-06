package semagrams.acsets.simple

import semagrams._
import acsets._
import acsets.abstr._
import semagrams.util._


import upickle.default._
import semagrams.acsets.simple.SimpleACSet.simpleACSetIsACSet
import semagrams.flatacsets.Petris.S





/* Traits */
sealed trait SchemaElt extends Generator:

// sealed trait SchemaGenerator extends SchemaElt with Generator:
  def schemaType: SchObs = this match
    case _:Table => TableOb
    case _:ValType[_] => ValTypeOb
    case _:FKey => FKeyOb
    case _:Column[_] => ColumnOb
  
  
  def asPart(): Part = Part(id,schemaType)
  

object SchemaElt:
  import Table.TableDef
  import ValType.TypeDef
  
  type EltDef = Table.TableDef | ValType.TypeDef
    | FKey.FKeyDef | Column.ColumnDef

  def apply(defn:EltDef): SchemaElt = defn match
    case elt:SchemaElt => elt
    case name:String => ValType.get(name) match
      case Some(vtype) => vtype
      case None => Table(name)
    case (str:String,t) => t match
      case t:String => SchemaElt("" -> (str,t))
      case t:Table => SchemaElt("" -> (str,t))
      case t:ValType[_] => SchemaElt("" -> (str,t))
      case (s:TableDef,t) => t match
        case t:String => ValType.get(t) match
          case Some(vtype) => Column(str,Table(s),vtype)(vtype.rw)
          case None => FKey(str,Table(s),Table(t))
        case t:Table => FKey(str,Table(s),t)
        case t:ValType[_] => Column(str,Table(s),t)(t.rw)
    case (s:Table,t:TableDef) => SchemaElt("" -> (s,t))
    case (s:Table,t:TypeDef) => SchemaElt("" -> (s,t))
    case baddef =>
      throw msgError(s"Bad SchemaElt definition $baddef")

      

    
    
  

sealed trait SchemaOb extends SchemaElt with Ob
sealed trait SchemaArrow extends SchemaElt with Arrow[Table]

/* Object generators */

case class Table(id:UUID,name:String)
  extends SchemaOb with GenOb
  derives ReadWriter



object Table:

  // def apply(id:UUID,name:String) = new Table(id,name)

  def apply(name:String) = new Table(UUID("Table"),name)

  type TableDef = String | Table
  def apply[T<:TableDef](t:T): Table = t match
    case s:String => Table(s)
    case t:Table => t



  
case class ValType[T:ReadWriter](id:UUID,name:String) 
  extends SchemaOb with GenType[T]:
  // def read(s:String) = fromStr[T](s)
  // def write(t:T): String = toStr[T](t)

  type ValueType = T
  def rw = readwriter[T]

object ValType:
  var valTypes: Map[UUID,ValType[_]] = Seq(
    ValType[String]("String"),
    ValType[Int]("Int"),
    ValType[Float]("Float"),
    ValType[Complex]("Complex"),
    ValType[Boolean]("Bool")
  ).map(vtype => vtype.id -> vtype).toMap

  def valStrings: Map[String,ValType[_]] = valTypes.values.map(vtype =>
    vtype.name -> vtype  
  ).toMap

  def register(typ:ValType[_]) = 
    valTypes += (typ.id -> typ)


  def get(id:UUID): Option[ValType[_]] = valTypes.get(id)
  def get(name:String): Option[ValType[_]] = valStrings.get(name)


  type TypeDef = String | ValType[_]
  def makeType[T:ReadWriter](name:String) = 
    val typ = new ValType[T](UUID("ValType"),name)
    register(typ)
    typ
  def makeType[T:ReadWriter](id:UUID,name:String) = 
    val typ = new ValType[T](id,name)
    register(typ)
    typ
  
  def apply[T:ReadWriter](name:String) = get(name).getOrElse(
    makeType[T](name)
  )
  def apply[T:ReadWriter](id:UUID,name:String) = get(id).getOrElse(
    get(name).getOrElse(
      makeType[T](id,name)
    )
  )
  
  def apply[Def<:TypeDef](defn:Def): ValType[_] = defn match
    case name:String => valStrings.get(name).getOrElse{
      println(s"Missing type value for ValType($name). Try adding a type argument ValType[_]($name)")
      makeType[Unit](name)
    }
    case t:ValType[_] => t

  
 


/* Arrow generators */
case class FKey(id:UUID,name:String,dom:Table,codom:Table) 
  extends SchemaArrow with GenHom[Table]

object FKey:

  def apply(name:String,dom:Table,codom:Table) =
    new FKey(UUID("FKey"),name,dom,codom) 
  import Table.TableDef
  type FKeyDef = (TableDef,TableDef) | (String,(TableDef,TableDef)) | FKey

  def apply(defn:FKeyDef): FKey = defn match
    case (s:TableDef,t:TableDef) => apply("" -> (s,t))
    case (name:String,(s:TableDef,t:TableDef)) =>
      new FKey(UUID("FKey"),name,Table(s),Table(t))
    case f:FKey => f
  
 
  

case class Column[T:ReadWriter](id:UUID,name:String,dom:Table,codom:ValType[T]) 
  extends GenAttr[Table,T] with SchemaArrow

object Column:

  import Table.TableDef
  import ValType.TypeDef
  type ColumnDef = (TableDef,TypeDef) | (String,(TableDef,TypeDef)) | Column[_]

  def apply[T:ReadWriter](name:String,s:Table,t:ValType[T]): Column[T] = 
    new Column[T](UUID("Column"),name,Table(s),t)

  def apply(defn:ColumnDef): Column[_] = defn match
    case (s:TableDef,t:TypeDef) => apply("" -> (s,t))
    case (name:String,(s:TableDef,t:TypeDef)) => 
      val typ = ValType(t)
      Column(name,Table(s),typ)(typ.rw)
    case c:Column[_] => c
  


  // def apply(nm:String,src:TableDef,tgt:ValTypeDef): Column[_] = 
  //   val typ = ValType(tgt)
  //   new Column(nm,Table(src),typ)(typ.rw)
  // def apply(src:TableDef,tgt:ValTypeDef): Column[_] =
  //   apply("",src,tgt)

  // def apply(defn:ColumnDef): Column[_] = defn match
  //   case (nm:String,(src,tgt)) => 
  //     val typ = ValType(tgt)
  //     apply(nm,src,typ)
  //   case (src:TableDef,prop:Property) => 
  //     val typ = ValType(prop)
  //     apply(prop.toString,src,typ)
  //   case (src:TableDef,tgt:ValTypeDef) => 
  //     apply("",src,tgt)

// type TableDef = String | Table

// type ValTypeDef = String | ValType[_] | Property
// type FKeyDef = (String, (TableDef, TableDef)) | (TableDef, TableDef)
// type ColumnDef = (String, (TableDef, ValTypeDef)) | (TableDef, ValTypeDef) | (TableDef,Property)

// type EltDef = TableDef | ValTypeDef | FKeyDef | ColumnDef
// object SchemaElt:
//   def apply(defn:EltDef): SchemaElt = defn match
//     case elt:SchemaElt => elt
//     case s:String => ValType.get(s) match
//       case Some(vtype) => vtype
//       case None => Table(s)
//     case (nm:String,(src:TableDef,tgt:String)) => ValType.get(tgt) match
//       case Some(vtype) => SchemaElt(nm -> (src,vtype))
//       case None => SchemaElt(nm -> (src,Table(tgt)))
//     case (nm:String,(src:TableDef,tgt:ValTypeDef)) => Column(nm,src,tgt)
//     case (nm:String,(src:TableDef,tgt:TableDef)) => FKey(nm,src,tgt)
//     case st:(Tuple2[_,_] & EltDef) => SchemaElt("" -> st) 
//     case st:(TableDef,ValTypeDef) => SchemaElt("" -> st) 
      
//     case x =>
//       throw msgError(s"unknown ACSetEltDef pattern $x")


/* Constructors */


/* Objects */

// // placeholder
// case class Query() extends SchemaOb:
//   def generators = Seq()
//   def label = "query"



// // placeholder
// case class ProdType() 
//   extends SchemaOb with TypeOb:
//   // type ScalaType = Unit
//   def generators = Seq()
//   def label = "prod"
//   type ValueType = Unit
//   val rw = readwriter[Unit]
//   // def read(s:String) = Some(())
//   // def write(ts: ScalaType): String = label


/* Arrows */







enum SchObs(val name:String) extends GenOb
  derives ReadWriter:
  case TableOb extends SchObs("TableOb")
  case ValTypeOb extends SchObs("ValTypeOb")
  case FKeyOb extends SchObs("FKeyOb")
  case ColumnOb extends SchObs("ColumnOb")
  val id = UUID("SchObs")
export SchObs._


enum SchHoms(val name:String,val dom:SchObs,val codom:SchObs) 
  extends GenHom[SchObs]:
  case FKeySrc extends SchHoms("FKeySrc",FKeyOb,TableOb)
  case FKeyTgt extends SchHoms("FKeyTgt",FKeyOb,TableOb)
  case ColumnSrc extends SchHoms("ColumnSrc",FKeyOb,TableOb)
  case ColumnTgt extends SchHoms("ColumnTgt",FKeyOb,ValTypeOb)
  val id = UUID("SchHoms")
export SchHoms._

// enum SchColumns(val name:String,val dom:SchObs)

case object SchSchema
val schSchemaIsSchema: Schema[SchSchema.type] = new Schema[SchSchema.type] {
  val name = "schSchemaIsSchema"

  val emptySchema = SchSchema
  type SchOb = SchObs
  type SchHom = SchHoms


  extension (s:SchSchema.type)
    def obMap = SchObs.values.map(ob => ob.id -> ob).toMap
    def homMap = SchHoms.values.map(f => f.id -> f).toMap
    def attrMap = Map()

    def renameElt(id0:UUID,name:String) = 
      println("SchSchema is static")
      s

    def _addElts(elts:Seq[Generator]) = 
      println("SchSchema is static")
      s

    def addProps(newProps:Seq[Property]) = 
      println("SchSchema is static")
      s

    def _remElts(elts:Seq[Generator]) = 
      println("SchSchema is static")
      s

    def remProps(oldProps:Seq[Property]) = 
      println("SchSchema is static")
      s



}





case class SchemaDisplay(
  globalProps: PropMap,
  props: Map[UUID,PropMap],
  selected: Seq[UUID]
):
  def setProps(id:UUID,newProps:PropMap) = this.copy(
    props = props + (id -> newProps)
  )


  def setProp(id:UUID,f:Property,v:f.Value) = this.copy(
    props = props + (id -> props(id).set(f,v))
  )


  def select(id:UUID) = this.copy(
    selected = (selected :+ id).distinct
  )
  def unselect(ids:UUID*) = this.copy(
    selected = selected.diff(ids)
  )
  def clear() = this.copy(
    selected = Seq()
  )

object SchemaDisplay:
  def apply(
    gps: PropMap,
    props:Map[UUID,PropMap],
    selected:Seq[UUID]
  ) = new SchemaDisplay(
    gps,
    props.withDefaultValue(PropMap()),
    selected
  )

  def apply(): SchemaDisplay = SchemaDisplay(PropMap(),Map(),Seq())





/* Schemas */


case class SimpleSchema(
  obMap:Map[UUID,Table] = Map(),
  homMap:Map[UUID,FKey] = Map(),
  attrMap:Map[UUID,Column[_]] = Map(),
  globalProps: Seq[Property] = Seq()
):

  override def toString = simpleSchemaIsSchema.display(this)

  def toACSet(displayData:SchemaDisplay): SimpleACSet[SchSchema.type] =

    type _S = SchSchema.type
    type _A = SimpleACSet[_S]
    implicit val schSchemaDef: Schema[_S] = schSchemaIsSchema
    implicit val acsetDef: ACSetWithSchema[_S][_A] = simpleACSetIsACSet(SchSchema)
    val ts = obMap.values.map(table =>
      Part(table.id,TableOb) -> displayData.props(table.id).set(Content,table.name)
    ).toSeq
    val vs = attrMap.values.toSeq.map(_.codom).distinct.map(vtype =>
      Part(vtype.id,ValTypeOb) -> displayData.props(vtype.id).set(Content,vtype.name)
    )
    val fs = homMap.values.map(fkey =>
      Part(fkey.id,FKeyOb) -> displayData.props(fkey.id)
        .set(FKeySrc,Part(fkey.dom.id,TableOb))
        .set(FKeyTgt,Part(fkey.codom.id,TableOb))
    ).toSeq
    val as = attrMap.values.map(column => 
      Part(column.id,ColumnOb) -> displayData.props(column.id)
        .set(ColumnSrc,Part(column.dom.id,TableOb))
        .set(ColumnTgt,Part(column.codom.id,ValTypeOb))
    ).toSeq

    ACSet[_S,_A](SchSchema).addParts(ts ++ vs ++ fs ++ as)._1
    


val schTables = Seq("Table","ValType","FKey","Column")
  .map(Table.apply)
val Seq(tableOb,valtypeOb,fkeyOb,columnOb) = schTables

val schFKeys = Seq(
  ("fkeySrc" -> ("FKey","Table")),
  ("fkeyTgt" -> ("FKey","Table")),
  ("columnSrc" -> ("Column","Table")),
  ("columnTgt" -> ("Column","ValType"))
).map(FKey.apply)
val Seq(fkeySrc,fkeyTgt,columnSrc,columnTgt) = schFKeys
val schSchema = SimpleSchema(
  schTables.map(table => table.id -> table).toMap,
  schFKeys.map(fkey => fkey.id -> fkey).toMap
)





val simpleSchemaIsSchema: Schema[SimpleSchema] = 
  new Schema[SimpleSchema]  {

    val name = "simpleSchemaIsSchema"
    val emptySchema = SimpleSchema()
    type SchOb = Table

    // type SchTable = Table
    type SchHom = FKey
    type SchAttr = Column[_]

    // type SchType =  ValType[_]
    // type SchemaElt = Table | FKey | Column[_]


    extension (s:SimpleSchema)
      def obMap = s.obMap
      def homMap = s.homMap
      def attrMap = s.attrMap

      def renameElt(id0:UUID,newName:String) = s.tryId(id0) match
        case Some(ob:SchOb) => s.copy(
          obMap = s.obMap + (id0 -> ob.copy(name = newName))
        )
        case Some(f:SchHom) => s.copy(
          homMap = s.homMap + (id0 -> f.copy(name = newName))
        )
        case Some(a:SchAttr) => s.copy(
          attrMap = s.attrMap + (id0 -> a.copy(name = newName)(a.codom.rw))
        )
        case None => s
      
        
      def _addElts(elts:Seq[Generator]) = s.copy(
        obMap = s.obMap ++ elts.collect{ case x:Table => x.id -> x},
        homMap = s.homMap ++ elts.collect{ case f:FKey => f.id -> f},
        attrMap = s.attrMap ++ elts.collect{ case a:Column[_] => a.id -> a}
      )

      def addProps(newProps:Seq[Property]) = s.copy(
        globalProps = (s.globalProps ++ newProps).distinct
      )

      def _remElts(elts:Seq[Generator]) = s.copy(
        obMap = s.obMap -- elts.collect{ case x:Table => x.id},
        homMap = s.homMap -- elts.collect{ case f:FKey => f.id},
        attrMap = s.attrMap -- elts.collect{ case a:Column[_] => a.id}
      )

      def remProps(oldProps:Seq[Property]) = s.copy(
        globalProps = (s.globalProps diff oldProps).distinct
      )


      // def generators = obs ++ homs ++ columns
  
}
object SimpleSchema:



  def apply(elts:SchemaElt*): SimpleSchema = 
      
    SimpleSchema(
      elts.collect{case t:Table => t.id -> t}.toMap,
      elts.collect{case f:FKey => f.id -> f}.toMap,
      elts.collect{case a:Column[_] => a.id -> a}.toMap,
      Seq()
    )
    

  
  def fromACSet[A:ACSetWithSchema[SchSchema.type]](schACSet:A): (SimpleSchema,SchemaDisplay) =
    


    val tableMap = schACSet.getProps(TableOb).toSeq.map( (part,props) => 
      part -> Table(part.id,props.get(Content).getOrElse(""))
    ).toMap

    val typeMap = schACSet.getProps(ValTypeOb).toSeq.
      map( (part:Part,props:PropMap) => ValType.get(part.id) match
        case Some(vtype) => part -> vtype
        case None => part -> props.get(Content)
          .flatMap(ValType.get(_))
          .getOrElse {
            println(s"Unknown valtype from part $part")
            ValType[Unit](props.get(Content).getOrElse(""))
          }
      ).toMap

    val fkeyMap = schACSet.getProps(FKeyOb).toSeq.collect { 
      case (part,props) if props.contains(FKeySrc,FKeyTgt) =>
        val s = props(FKeySrc)
        val t = props(FKeyTgt)

        part -> FKey(
          props.get(Content).getOrElse(""),
          tableMap(s),
          tableMap(t)
        )
    }.toMap

    val columnMap = schACSet.getProps(ColumnOb).toSeq.collect { 
      case (part,props) if props.contains(ColumnSrc,ColumnTgt) =>
        val s = props(ColumnSrc)
        val typ = typeMap(props(ColumnTgt))

        part -> Column(
          props.get(Content).getOrElse(""),
          tableMap(s),
          typ
        )(typ.rw)
    }.toMap


    val partsMap:Map[Part,SchemaElt] = tableMap ++ typeMap ++ fkeyMap ++ columnMap

    val schemaProps:Map[UUID,PropMap] = partsMap.map( (part,elt) =>
      elt.id -> schACSet.getProps(part)  
    )
    
    val sch = SimpleSchema(partsMap.values.toSeq:_*)
    val disp = SchemaDisplay(
      PropMap(),
      schemaProps,
      Seq()
    )


    (sch,disp)




          

    

