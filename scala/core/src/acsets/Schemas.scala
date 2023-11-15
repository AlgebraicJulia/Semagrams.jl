package semagrams.acsets



import semagrams._  
import semagrams.acsets._
import semagrams.util._




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

  def diff(sch2:Schema) = this.elts -- sch2.elts.keys

    
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
  def homs: Map[UUID,Hom[_,_]] = elts.collect{ case kv:Tuple2[UUID,Hom[_,_]] => kv }

  
  def eltSeq: Seq[Elt] = elts.values.toSeq

  def tableSeq: Seq[Table] = eltSeq.collect{ case t:Table => t }
  def fkeySeq: Seq[FKey] = eltSeq.collect{ case f:FKey => f }
  def attrSeq: Seq[Attr[_]] = eltSeq.collect{ case a:Attr[_] => a}
 
  def obSeq: Seq[Ob] = eltSeq.collect{ case ob:Ob => ob}
  def homSeq: Seq[Hom[_,_]] = eltSeq.collect{ case f:Hom[_,_] => f }
  
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

    

object BasicSchema:



  def apply[E<:(Elt|Property)](elts:E*): BasicSchema = new BasicSchema(
    UUID("BasicSchema"),
    elts.collect{ case elt:Elt => elt}.eltMap,
    elts.collect{ case f:Property => f}
  )
      