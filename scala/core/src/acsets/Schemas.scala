package semagrams.acsets

import scala.annotation.targetName


import semagrams._  
import semagrams.acsets._
import semagrams.util._


// import semagrams.acsets.simple.BasicSchema
// import semagrams.acsets.simple.simpleSchemaIsSchema

// import scala.language.implicitConversions

// import upickle.default._




trait Schema:

  val id: UUID
  var name: String

  override def toString = if name == ""
    then display else name

  /* Implementation API */
  def globalProps: Seq[Property]
  def elts: Map[UUID,Elt]

  def isDynamic: Boolean = this match
    case s:DynamicSchema => true
    case _ => false

    

    
  /* Generic methods */
  
  /* Global Properties */

  def hasProp(f:Property): Boolean = globalProps.contains(f)
  def hasProps(fs:Seq[Property]): Boolean = fs.forall(globalProps.contains)
  def contains(fs:Property*): Boolean = fs.forall(globalProps.contains)

  /* Schema elements */
  

  def tables: Map[UUID,Table] = elts.collect{ case kv:Tuple2[UUID,Table] => kv }
  def fkeys: Map[UUID,FKey] = elts.collect{ case kv:Tuple2[UUID,FKey] => kv }
  def attrs: Map[UUID,Attr[_]] = elts.collect{ case kv:Tuple2[UUID,Attr[_]] => kv }

  def obs: Map[UUID,Ob] = elts.collect{ case kv:Tuple2[UUID,Ob] => kv }
  def arrows: Map[UUID,Hom[_,_]] = elts.collect{ case kv:Tuple2[UUID,Hom[_,_]] => kv }

  
  def eltSeq: Seq[Elt] = elts.values.toSeq

  def tableSeq: Seq[Table] = eltSeq.collect{ case t:Table => t }
  def fkeySeq: Seq[FKey] = eltSeq.collect{ case f:FKey => f }
  def attrSeq: Seq[Attr[_]] = eltSeq.collect{ case a:Attr[_] => a}
 
  def obSeq: Seq[Ob] = eltSeq.collect{ case ob:Ob => ob}
  def arrowSeq: Seq[Hom[_,_]] = eltSeq.collect{ case f:Hom[_,_] => f }
  
  /* Check if `id0` is contained in `s` */
  def hasId(id0:UUID): Boolean = elts.contains(id0)
  def hasTable(id0:UUID): Boolean = tables.contains(id0)
  def hasFKey(id0:UUID): Boolean = fkeys.contains(id0)
  def hasAttr(id0:UUID): Boolean = attrs.contains(id0)
  def hasHom(id0:UUID): Boolean = (fkeys ++ attrs).contains(id0)

  /* Check if `elt` is contained in `s` */
  def hasElt(elt:Elt): Boolean = eltSeq.contains(elt)
  def hasTable(t:Table): Boolean = tableSeq.contains(t)
  def hasFKey(f:FKey): Boolean = fkeySeq.contains(f)
  def hasAttr(a:Attr[_]): Boolean = attrSeq.contains(a)
  def hasHom(f:Hom[_,_]): Boolean = (fkeySeq ++ attrSeq).contains(f)

  def hasIds(ids:Iterable[UUID]) = ids.toSet.subsetOf(elts.keySet)
  def hasElts(elts:Iterable[Elt]) = elts.forall(hasElt)

  def tryIds(ids:Iterable[UUID]) = ids.map(tryElt)
  def tryTables(ids:Iterable[UUID]) = ids.map(tryTable)
  def tryFKeys(ids:Iterable[UUID]) = ids.map(tryFKey)
  def tryAttrs(ids:Iterable[UUID]) = ids.map(tryAttr)

  def getIds(ids:Iterable[UUID]) = ids.map(getElt)
  def getTables(ids:Iterable[UUID]) = ids.map(getTable)
  def getFKeys(ids:Iterable[UUID]) = ids.map(getFKey)
  def getAttrs(ids:Iterable[UUID]) = ids.map(getAttr)

  def collectIds(ids:Iterable[UUID]) = ids.collect(tryElt.unlift)
  def collectTables(ids:Iterable[UUID]) = ids.collect(tryTable.unlift)
  def collectFKeys(ids:Iterable[UUID]) = ids.collect(tryFKey.unlift)
  def collectAttrs(ids:Iterable[UUID]) = ids.collect(tryAttr.unlift)


  /* Return an optional element associated with `id0` */
  def tryElt(id0:UUID): Option[Elt] = elts.get(id0)
  def tryTable(id0:UUID): Option[Table] = tables.get(id0)
  def tryFKey(id0:UUID): Option[FKey] = fkeys.get(id0)
  def tryAttr(id0:UUID): Option[Attr[_]] = attrs.get(id0)

  /* Return the element associated with `id0` (unsafe) */
  def getElt(id0:UUID): Elt = elts(id0)
  def getTable(id0:UUID): Table = tables(id0)
  def getFKey(id0:UUID): FKey = fkeys(id0)
  def getAttr(id0:UUID): Attr[_] = attrs(id0)




  def display = "Schema(" + 
    (if tableSeq.isEmpty then "" else 
      "\n  Table:   " + tableSeq.mkString(", ")
    ) + (if fkeySeq.isEmpty then "" else 
      "\n  Hom:  " + fkeySeq.mkString(", ")
    ) + (if attrSeq.isEmpty then "" else 
      "\n  Observable: " + attrSeq.mkString(", ")
    ) + (if globalProps.isEmpty then "" else 
      "\n  Prop: " + globalProps.mkString(", ")
    ) + (if (elts ++ globalProps).isEmpty 
      then ")\n" else "\n)\n"
    )




trait DynamicSchema extends Schema:

  def addGlobalProps(props:Iterable[Property]): DynamicSchema
  def remGlobalProps(props:Iterable[Property]): DynamicSchema

  def addElts(elts:Iterable[Elt]): DynamicSchema
  def remElts(ids:Iterable[UUID],cascade:Boolean): DynamicSchema
  def modTable(id0:UUID,f:Table => Table): DynamicSchema
  def modFKey(id0:UUID,f:FKey => FKey): DynamicSchema
  def modAttr(id0:UUID,f:Attr[_] => Attr[_]): DynamicSchema


    /* Generic methods */
    
    /* Global Properties */
    // def +(f:Property) = addProps(Seq(f))
    // @targetName("plusProps")
    // def ++(fs:Iterable[Property]) = addGlobalProps(fs.toSeq)
    // def -(f:Property) = remGlobalProps(Seq(f))
    // def --(fs:Iterable[Property]) = remGlobalProps(fs.toSeq)

    /* Adding schema elements */
    def +(elt:Elt): DynamicSchema = addElts(Seq(elt))
    def ++(elts:Iterable[Elt]): DynamicSchema = addElts(elts.toSeq)
    def ++(s:DynamicSchema): DynamicSchema = 
      addGlobalProps(s.globalProps).addElts(s.eltSeq)



object Schema:
  def apply(elts:Elt | Property*): Schema = BasicSchema(elts:_*)







case class BasicSchema(
  id:UUID,
  elts:Map[UUID,Elt] = Map(),
  globalProps: Seq[Property] = Seq()
) extends DynamicSchema:

  var name = ""


  def addGlobalProps(props:Iterable[Property]) = copy(
    globalProps = (globalProps ++ props.toSeq).distinct
  )
  def remGlobalProps(props:Iterable[Property]) = copy(
    globalProps = globalProps diff props.toSeq
  )

  def addElts(newElts:Iterable[Elt]) = this.copy(
    elts = elts ++ newElts.eltMap
  )

  def remElts(ids:Iterable[UUID],cascade:Boolean = true) = if cascade
    then this.copy(
      elts = elts.filterNot(_._2.uses(ids)),
    )
    else this.copy(
      elts = elts -- ids,
    )


  def modTable(id0: UUID, mod: Table => Table): DynamicSchema =
    tables.get(id0) match
      case Some(t) => this + mod(t)
      case None => 
        println(s"modTable: Missing id $id0")
        this
    
  def modFKey(id0: UUID, mod: FKey => FKey): DynamicSchema =
    fkeys.get(id0) match
      case Some(f) => this + mod(f)
      case None => 
        println(s"modFKey: Missing id $id0")
        this
    
  def modAttr(id0: UUID, mod: Attr[_] => Attr[_]): DynamicSchema =
    attrs.get(id0) match
      case Some(a) => this + mod(a)
      case None => 
        println(s"modAttr: Missing id $id0")
        this

    


          
  // def modFKey(id0:UUID,f:FKey => FKey): Schema =
  //   fkeys.get(id0) match
  //     case Some(ff) => this + f(ff)
  //     case None =>
  //       println(s"Missing id $id0")
  //       this
          
  // def modAttr(id0:UUID,f:Attr[_] => Attr[_]): Schema =
  //   attrs.get(id0) match
  //     case Some(a) => this + f(a)
  //     case None =>
  //       println(s"Missing id $id0")
  //       this
            
      // def obMap = s.obMap
      // def homMap = s.homMap
      // def attrMap = s.attrMap

      // def renameElt(id0:UUID,newName:String) = s.tryId(id0) match
      //   case Some(ob:SchOb) => s.copy(
      //     obMap = s.obMap + (id0 -> ob.copy(name = newName))
      //   )
      //   case Some(f:SchHom) => s.copy(
      //     homMap = s.homMap + (id0 -> f.copy(name = newName))
      //   )
      //   case Some(a:SchAttr) => s.copy(
      //     attrMap = s.attrMap + (id0 -> a.copy(name = newName)(a.codom.rw))
      //   )
      //   case None => s
      
        
      // def _addElts(elts:Seq[Generator]) = s.copy(
      //   obMap = s.obMap ++ elts.collect{ case x:Table => x.id -> x},
      //   homMap = s.homMap ++ elts.collect{ case f:FKey => f.id -> f},
      //   attrMap = s.attrMap ++ elts.collect{ case a:Column[_] => a.id -> a}
      // )

      // def addProps(newProps:Seq[Property]) = s.copy(
      //   globalProps = (s.globalProps ++ newProps).distinct
      // )

      // def _remElts(elts:Seq[Generator]) = s.copy(
      //   obMap = s.obMap -- elts.collect{ case x:Table => x.id},
      //   homMap = s.homMap -- elts.collect{ case f:FKey => f.id},
      //   attrMap = s.attrMap -- elts.collect{ case a:Column[_] => a.id}
      // )

      // def remProps(oldProps:Seq[Property]) = s.copy(
      //   globalProps = (s.globalProps diff oldProps).distinct
      // )


      // def generators = obs ++ homs ++ columns
  
// }
object BasicSchema:



  def apply(elts:Elt | Property*): BasicSchema = new BasicSchema(
    UUID("BasicSchema"),
    elts.collect{ case elt:Elt => elt}.eltMap,
    elts.collect{ case f:Property => f}
  )
      
//     BasicSchema(
//       elts.collect{case t:Table => t.id -> t}.toMap,
//       elts.collect{case f:FKey => f.id -> f}.toMap,
//       elts.collect{case a:Column[_] => a.id -> a}.toMap,
//       Seq()
//     )
    

  
//   def fromACSet[A:ACSetWithSchema[SchSchema.type]](schACSet:A): (BasicSchema,SchemaDisplay) =
    


//     val tableMap = schACSet.getProps(TableOb).toSeq.map( (part,props) => 
//       part -> Table(part.id,props.get(Content).getOrElse(""))
//     ).toMap

//     val typeMap = schACSet.getProps(ValTypeOb).toSeq.
//       map( (part:Part,props:PropMap) => ValType.get(part.id) match
//         case Some(vtype) => part -> vtype
//         case None => part -> props.get(Content)
//           .flatMap(ValType.get(_))
//           .getOrElse {
//             println(s"Unknown valtype from part $part")
//             ValType[Unit](props.get(Content).getOrElse(""))
//           }
//       ).toMap

//     val fkeyMap = schACSet.getProps(FKeyOb).toSeq.collect { 
//       case (part,props) if props.contains(FKeySrc,FKeyTgt) =>
//         val s = props(FKeySrc)
//         val t = props(FKeyTgt)

//         part -> FKey(
//           props.get(Content).getOrElse(""),
//           tableMap(s),
//           tableMap(t)
//         )
//     }.toMap

//     val columnMap = schACSet.getProps(ColumnOb).toSeq.collect { 
//       case (part,props) if props.contains(ColumnSrc,ColumnTgt) =>
//         val s = props(ColumnSrc)
//         val typ = typeMap(props(ColumnTgt))

//         part -> Column(
//           props.get(Content).getOrElse(""),
//           tableMap(s),
//           typ
//         )(typ.rw)
//     }.toMap


//     val partsMap:Map[Part,SchemaElt] = tableMap ++ typeMap ++ fkeyMap ++ columnMap

//     val schemaProps:Map[UUID,PropMap] = partsMap.map( (part,elt) =>
//       elt.id -> schACSet.getProps(part)  
//     )
    
//     val sch = BasicSchema(partsMap.values.toSeq:_*)
//     val disp = SchemaDisplay(
//       PropMap(),
//       schemaProps,
//       Seq()
//     )


//     (sch,disp)




          

    

// /* Traits */
// sealed trait SchemaElt extends Generator:

// // sealed trait SchemaGenerator extends SchemaElt with Generator:
//   def schemaType: SchObs = this match
//     case _:Table => TableOb
//     case _:ValType[_] => ValTypeOb
//     case _:FKey => FKeyOb
//     case _:Column[_] => ColumnOb
  
  
//   def asPart(): Part = Part(id,schemaType)
  

// object SchemaElt:
//   import Table.TableDef
//   import ValType.TypeDef
  
//   type EltDef = Table.TableDef | ValType.TypeDef
//     | FKey.FKeyDef | Column.ColumnDef

//   def apply(defn:EltDef): SchemaElt = defn match
//     case elt:SchemaElt => elt
//     case name:String => ValType.get(name) match
//       case Some(vtype) => vtype
//       case None => Table(name)
//     case (str:String,t) => t match
//       case t:String => SchemaElt("" -> (str,t))
//       case t:Table => SchemaElt("" -> (str,t))
//       case t:ValType[_] => SchemaElt("" -> (str,t))
//       case (s:TableDef,t) => t match
//         case t:String => ValType.get(t) match
//           case Some(vtype) => Column(str,Table(s),vtype)(vtype.rw)
//           case None => FKey(str,Table(s),Table(t))
//         case t:Table => FKey(str,Table(s),t)
//         case t:ValType[_] => Column(str,Table(s),t)(t.rw)
//     case (s:Table,t:TableDef) => SchemaElt("" -> (s,t))
//     case (s:Table,t:TypeDef) => SchemaElt("" -> (s,t))
//     case baddef =>
//       throw msgError(s"Bad SchemaElt definition $baddef")

      

    
    
  

// sealed trait SchemaOb extends SchemaElt with Ob
// sealed trait SchemaArrow extends SchemaElt with Hom[Table]

// /* Object generators */

// case class Table(id:UUID,name:String)
//   extends SchemaOb with GenOb
//   derives ReadWriter



// object Table:

//   // def apply(id:UUID,name:String) = new Table(id,name)

//   def apply(name:String) = new Table(UUID("Table"),name)

//   type TableDef = String | Table
//   def apply[T<:TableDef](t:T): Table = t match
//     case s:String => Table(s)
//     case t:Table => t



  
// case class ValType[T:ReadWriter](id:UUID,name:String) 
//   extends SchemaOb with GenType[T]:
//   // def read(s:String) = fromStr[T](s)
//   // def write(t:T): String = toStr[T](t)

//   type ValueType = T
//   def rw = readwriter[T]

// object ValType:
//   var valTypes: Map[UUID,ValType[_]] = Seq(
//     ValType[String]("String"),
//     ValType[Int]("Int"),
//     ValType[Float]("Float"),
//     ValType[Complex]("Complex"),
//     ValType[Boolean]("Bool")
//   ).map(vtype => vtype.id -> vtype).toMap

//   def valStrings: Map[String,ValType[_]] = valTypes.values.map(vtype =>
//     vtype.name -> vtype  
//   ).toMap

//   def register(typ:ValType[_]) = 
//     valTypes += (typ.id -> typ)


//   def get(id:UUID): Option[ValType[_]] = valTypes.get(id)
//   def get(name:String): Option[ValType[_]] = valStrings.get(name)


//   type TypeDef = String | ValType[_]
//   def makeType[T:ReadWriter](name:String) = 
//     val typ = new ValType[T](UUID("ValType"),name)
//     register(typ)
//     typ
//   def makeType[T:ReadWriter](id:UUID,name:String) = 
//     val typ = new ValType[T](id,name)
//     register(typ)
//     typ
  
//   def apply[T:ReadWriter](name:String) = get(name).getOrElse(
//     makeType[T](name)
//   )
//   def apply[T:ReadWriter](id:UUID,name:String) = get(id).getOrElse(
//     get(name).getOrElse(
//       makeType[T](id,name)
//     )
//   )
  
//   def apply[Def<:TypeDef](defn:Def): ValType[_] = defn match
//     case name:String => valStrings.get(name).getOrElse{
//       println(s"Missing type value for ValType($name). Try adding a type argument ValType[_]($name)")
//       makeType[Unit](name)
//     }
//     case t:ValType[_] => t

  
 


// /* Arrow generators */
// case class FKey(id:UUID,name:String,dom:Table,codom:Table) 
//   extends SchemaArrow with GenHom[Table]

// object FKey:

//   def apply(name:String,dom:Table,codom:Table) =
//     new FKey(UUID("FKey"),name,dom,codom) 
//   import Table.TableDef
//   type FKeyDef = (TableDef,TableDef) | (String,(TableDef,TableDef)) | FKey

//   def apply(defn:FKeyDef): FKey = defn match
//     case (s:TableDef,t:TableDef) => apply("" -> (s,t))
//     case (name:String,(s:TableDef,t:TableDef)) =>
//       new FKey(UUID("FKey"),name,Table(s),Table(t))
//     case f:FKey => f
  
 
  

// case class Column[T:ReadWriter](id:UUID,name:String,dom:Table,codom:ValType[T]) 
//   extends GenAttr[Table,T] with SchemaArrow

// object Column:

//   import Table.TableDef
//   import ValType.TypeDef
//   type ColumnDef = (TableDef,TypeDef) | (String,(TableDef,TypeDef)) | Column[_]

//   def apply[T:ReadWriter](name:String,s:Table,t:ValType[T]): Column[T] = 
//     new Column[T](UUID("Column"),name,Table(s),t)

//   def apply(defn:ColumnDef): Column[_] = defn match
//     case (s:TableDef,t:TypeDef) => apply("" -> (s,t))
//     case (name:String,(s:TableDef,t:TypeDef)) => 
//       val typ = ValType(t)
//       Column(name,Table(s),typ)(typ.rw)
//     case c:Column[_] => c
  


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




    // def addTables(ts:Iterable[Table]): Schema = addElts(ts)
    // @targetName("addTablesIdName")
    // def addTables(kvs:Iterable[(UUID,String)]): S =
    //   s.addElts(kvs.map((id,name) => Table(id,name)))
    // @targetName("addTablesById")
    // def addTables(ids:Iterable[UUID]): S = s.addTables(ids.map(_ -> ""))

    // def addTable(t:Table) = s.addTables(Seq(t))
    // def addTable(id0:UUID) = s.addTables(Seq(id0))
    // def addTable(id0:UUID,name:String) = s.addTables(Seq(id0 -> name))
    // def getOrAddTable(id:UUID,name:String = "") = s.tryTable(id) match
    //   case Some(t) => t
    //   case None => s.addTable(id,name)

    // def addFKeys(fs:Iterable[FKey]): S = s.addElts(fs)
    // @targetName("addFKeysIdName")
    // def addFKeys(kvs:Iterable[(UUID,(String,UUID,UUID))]): S =
    //   s.addElts(kvs.map{ case (id,(name,domId,codomId)) =>
    //     val dom = s.
    //   })
    //     kvs.map((id,name) => (id,name)))
    // @targetName("addFKeysById")
    // def addFKeys(ids:Iterable[UUID]): S = s.addFKeys(ids.map(_ -> ""))

    // def addFKey(f:FKey) = s.addFKeys(Seq(f))
    // def addFKey(id0:UUID) = s.addFKeys(Seq(id0))
    // def addFKey(id0:UUID,name:String) = s.addFKeys(Seq(id0 -> name))
    // def getOrAddFKey(id:UUID,name:String = "") = s.tryFKey(id) match
    //   case Some(f) => f
    //   case None => s.addFKey(id,name)




    // def addFKeys(fs:Iterable[FKey]): S = s.addElts(fs)
    // @targetName("addFKeysIdName")
    // def addFKeys(kvs:Iterable[(UUID,(String,UUID,UUID))]): S =
    //   s.addElts(kvs.map{ case (id,(name,domId,codomId)) => 
    //     FKey() })
    // @targetName("addTablesById")
    // def addTables(ids:Iterable[UUID]): S = s.addTables(ids.map(_ -> ""))

    // def addTables(ts:Iterable[Table]): S = s.addElts(ts)
    // @targetName("addTablesIdName")
    // def addTables(kvs:Iterable[(UUID,String)]): S =
    //   s.addElts(kvs.map((id,name) => Table(id,name)))
    // @targetName("addTablesById")
    // def addTables(ids:Iterable[UUID]): S = s.addTables(ids.map(_ -> ""))



    // def addFKeys(fkeys:Seq[FKey]): S
    // def remFKeys(ids:Seq[UUID]): S
    // def modFKey(id:UUID,f:FKey => FKey): S

    // def addAttrs(attrSeq:Seq[Attr[_]]): S
    // def remAttrs(ids:Seq[UUID]): S
    // def modAttr[T0:ReadWriter,T1:ReadWriter](id:UUID,f:Attr[T0] => Attr[T1]): S



    // def addElts(elts:Iterable[Elt]): S = 
    //   val (ts,fs,as) = elts.foldLeft(
    //     (Seq[Table](),Seq[FKey](),Seq[Attr[_]]())
    //   ){ case ((ts,fs,as),elt) => elt match
    //     case t:Table => (ts :+ t,fs,as)
    //     case f:FKey => (ts,fs :+ f,as)
    //     case a:Attr[_] => (ts,fs,as :+ a)
    //   }
    //   addTables(ts).addFKeys(fs).addAttrs(as)

    // def addElt(elt:Elt): S =
    //   addElts(elt.generators) 
 
    // def addProp(prop:Property): S = 
    //   addProps(Seq(prop))


    // def remElt(elt:Elt) = s._remElts(elt.generators)
    // def remElts(elts:Seq[Elt]) = s._remElts(elts.flatMap(_.generators))

    // def remId(id:UUID) = s.tryId(id) match
    //   case Some(elt) => s.remElt(elt)
    //   case None => s

    // def remIds(ids:Seq[UUID]) = s.remElts(ids.flatMap(id => s.tryId(id)))



    // def +(elt:Elt) = s.addElt(elt)
    // def +(prop:Property) = s.addProp(prop)
    // def ++(elts:Seq[Elt]) = s.addElts(elts)
    // def ++[SS:Schema](other:SS) = 
    //   s.addElts(other.generators)

    // def --(elts:Seq[Elt]): Seq[Generator] = generators.diff(elts)
    // def --(that:S): Seq[Generator] = --(that.generators)


    // def ++(stuff:Seq[AElt | Property]): S = addElts(
    //   stuff.collect{ case x:AElt => x}
    //     .flatMap(_.generators)
    //   ).addProps(stuff.collect{ case p:Property => p})

    // def ++(s:S): S = ++(s.generators) ++ s.props


