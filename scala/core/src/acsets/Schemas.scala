package semagrams.acsets


import semagrams._
// import semagrams.util.UID
// import semagrams._  
import semagrams.acsets._
import semagrams.util._
// // import semagrams.{FKey, Hom, given, Ob, Elt, Attr, Table, Property}




trait Schema:

  val id: UID
  var name: String

  override def toString = if name == ""
    then display else name

  /* Implementation API */
  def globalProps: Seq[Property]
  def elts: Map[UID,Elt]

  def isDynamic: Boolean = this match
    case s:DynamicSchema => true
    case _ => false

  def diff(sch2:Schema) = this.elts -- sch2.elts.keys

    
  /* Generic methods */
  
  /* Global Properties */

  def hasProp(f:Property): Boolean = globalProps.contains(f)
  def hasProps(fs:Seq[Property]): Boolean = fs.forall(globalProps.contains)
  def contains(fs:Property*): Boolean = fs.forall(globalProps.contains)

  /* Schema elements */
  

  def tables: Map[UID,Table] = elts.collect{ case kv:Tuple2[UID,Table] => kv }
  def fkeys: Map[UID,FKey] = elts.collect{ case kv:Tuple2[UID,FKey] => kv }
  def attrs: Map[UID,Attr[_]] = elts.collect{ case kv:Tuple2[UID,Attr[_]] => kv }

  def obs: Map[UID,Ob] = elts.collect{ case kv:Tuple2[UID,Ob] => kv }
  def homs: Map[UID,Hom[_,_]] = elts.collect{ case kv:Tuple2[UID,Hom[_,_]] => kv }

  
  def eltSeq: Seq[Elt] = elts.values.toSeq

  def tableSeq: Seq[Table] = eltSeq.collect{ case t:Table => t }
  def fkeySeq: Seq[FKey] = eltSeq.collect{ case f:FKey => f }
  def attrSeq: Seq[Attr[_]] = eltSeq.collect{ case a:Attr[_] => a}
 
  def obSeq: Seq[Ob] = eltSeq.collect{ case ob:Ob => ob}
  def homSeq: Seq[Hom[_,_]] = eltSeq.collect{ case f:Hom[_,_] => f }
  
  /* Check if `id0` is contained in `s` */
  def hasId(id0:UID): Boolean = elts.contains(id0)
  def hasTable(id0:UID): Boolean = tables.contains(id0)
  def hasFKey(id0:UID): Boolean = fkeys.contains(id0)
  def hasAttr(id0:UID): Boolean = attrs.contains(id0)
  def hasHom(id0:UID): Boolean = (fkeys ++ attrs).contains(id0)

  /* Check if `elt` is contained in `s` */
  def hasElt(elt:Elt): Boolean = eltSeq.contains(elt)
  def hasTable(t:Table): Boolean = tableSeq.contains(t)
  def hasFKey(f:FKey): Boolean = fkeySeq.contains(f)
  def hasAttr(a:Attr[_]): Boolean = attrSeq.contains(a)
  def hasHom(f:Hom[_,_]): Boolean = (fkeySeq ++ attrSeq).contains(f)

  def hasIds(ids:Iterable[UID]) = ids.toSet.subsetOf(elts.keySet)
  def hasElts(elts:Iterable[Elt]) = elts.forall(hasElt)

  def tryIds(ids:Iterable[UID]) = ids.map(tryElt)
  def tryTables(ids:Iterable[UID]) = ids.map(tryTable)
  def tryFKeys(ids:Iterable[UID]) = ids.map(tryFKey)
  def tryAttrs(ids:Iterable[UID]) = ids.map(tryAttr)

  def getIds(ids:Iterable[UID]) = ids.map(getElt)
  def getTables(ids:Iterable[UID]) = ids.map(getTable)
  def getFKeys(ids:Iterable[UID]) = ids.map(getFKey)
  def getAttrs(ids:Iterable[UID]) = ids.map(getAttr)

  def collectIds(ids:Iterable[UID]) = ids.collect(tryElt.unlift)
  def collectTables(ids:Iterable[UID]) = ids.collect(tryTable.unlift)
  def collectFKeys(ids:Iterable[UID]) = ids.collect(tryFKey.unlift)
  def collectAttrs(ids:Iterable[UID]) = ids.collect(tryAttr.unlift)


  /* Return an optional element associated with `id0` */
  def tryElt(id0:UID): Option[Elt] = elts.get(id0)
  def tryTable(id0:UID): Option[Table] = tables.get(id0)
  def tryFKey(id0:UID): Option[FKey] = fkeys.get(id0)
  def tryAttr(id0:UID): Option[Attr[_]] = attrs.get(id0)

  /* Return the element associated with `id0` (unsafe) */
  def getElt(id0:UID): Elt = elts(id0)
  def getTable(id0:UID): Table = tables(id0)
  def getFKey(id0:UID): FKey = fkeys(id0)
  def getAttr(id0:UID): Attr[_] = attrs(id0)




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
  def remElts(ids:Iterable[UID],cascade:Boolean): DynamicSchema
  def modTable(id0:UID,f:Table => Table): DynamicSchema
  def modFKey(id0:UID,f:FKey => FKey): DynamicSchema
  def modAttr(id0:UID,f:Attr[_] => Attr[_]): DynamicSchema


    /* Generic methods */
    
    /* Adding schema elements */
    def +(elt:Elt): DynamicSchema = addElts(Seq(elt))
    def ++(elts:Iterable[Elt]): DynamicSchema = addElts(elts.toSeq)
    def ++(s:DynamicSchema): DynamicSchema = 
      addGlobalProps(s.globalProps).addElts(s.eltSeq)



object Schema:
  // def apply(elts:Elt | Property*): Schema = BasicSchema(UID("Schema"),elts:_*)
  def apply(id:UID,elts:Elt | Property*): Schema = BasicSchema(id:UID,elts:_*)







case class BasicSchema(
  id:UID,
  elts:Map[UID,Elt] = Map(),
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

  def remElts(ids:Iterable[UID],cascade:Boolean = true) = if cascade
    then this.copy(
      elts = elts.filterNot(_._2.uses(ids)),
    )
    else this.copy(
      elts = elts -- ids,
    )


  def modTable(id0: UID, mod: Table => Table): DynamicSchema =
    tables.get(id0) match
      case Some(t) => this + mod(t)
      case None => 
        println(s"modTable: Missing id $id0")
        this
    
  def modFKey(id0: UID, mod: FKey => FKey): DynamicSchema =
    fkeys.get(id0) match
      case Some(f) => this + mod(f)
      case None => 
        println(s"modFKey: Missing id $id0")
        this
    
  def modAttr(id0: UID, mod: Attr[_] => Attr[_]): DynamicSchema =
    attrs.get(id0) match
      case Some(a) => this + mod(a)
      case None => 
        println(s"modAttr: Missing id $id0")
        this

    

object BasicSchema:



  def apply[E<:(Elt|Property)](id:UID,elts:E*): BasicSchema = new BasicSchema(
    id,
    elts.collect{ case elt:Elt => elt}.eltMap,
    elts.collect{ case f:Property => f}
  )
      