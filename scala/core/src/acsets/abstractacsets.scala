package semagrams.acsets.abstr



import semagrams._
import semagrams.acsets._

import upickle.default._
import semagrams.util.UUID
import semagrams.acsets.simple.SchemaElt




type ACSetOn = [X] =>> [A] =>> ACSet[A]{ type SchOb = X }

// def f(x:ACSetOn[Int,SimpleACSet]) = x.globalData


// val acsetTypes[Data:]

type ACSetWithData[D] = [A] =>> ACSet[A]{ type Data = D }
// type ACSetWithOb[X<:Ob] = [A] =>> ACSet[A]{ type ACSetOb >: X }
// type ACSetWithObBound[X<:Ob] = [A] =>> ACSet[A]{ type ACSetOb <: X }
// type ACSetWithObAndData[X<:Ob,D] = [A] =>> ACSet[A] {
//   type Data = D
//   type ACSetOb >: X
// }

type ACSetWithSchema[S] = [A] =>> ACSet[A]{ type Sch = S }

type ACSetWithSchemaAndData[S,D] = [A] =>> ACSetWithSchema[S][A] & ACSetWithData[D][A]




trait ACSet[A] {

  val name: String
  override def toString = name


  type Data
  implicit val dataIsPartData:PartData[Data]

  type _ACSetOb <: Ob
  type ACSetOb = _ACSetOb & Matchable
  type Sch
  // val schemaIsSchema: SchemaWithOb[ACSetOb][Sch]
  implicit val schemaIsSchema: Schema[Sch]

  def fromSchema(s:Sch): A

  // def data(props:PropMap = PropMap()) = dataIsPartData.toData(props)
  // import schemaIsSchema._
  // type Ob <
  // val obIsElt:AbstractElt[Ob]
  //   // ACSetOb<:AOb,
  //   // Data:PartData,
  //   // S <:AbstractSchema[ACSetOb],
  //   // A// <:AbstractACSet[ACSetOb,Data,S,A]


  // (val schema:S) { self:A =>
  // implicit val dataIsPartData:PartData[Data]

  // val partData = summon[PartData[Data]]
  // val schema: AbstractSchema
  // import schema._
  // // type ACSetOb = schema.SchTable
  // type Part = Part

  extension (a:A) {
    /* Global properties */
    def schema: Sch
    def globalData: Data
    def setGlobalProp(f:Property,v:f.Value): A
    def remGlobalProp(f:Property): A
    def addSchemaElts(elts:Seq[Elt]): A
    def remSchemaElts(ids:Seq[UUID]): A
    def replaceSchemaElts(elts:Seq[(UUID,Elt)]): A

    // def setSchema(newSchema:Sch): A
    
    /* Manipulating parts */
    def getParts(ob:Ob): Seq[Part]
    def getData(part:Part): Data

    /* Manipulating parts */
    def _addParts(data:Seq[(Part,Data)]): (A,Seq[Part])  
    def remParts(ps:Seq[Part]): A
    def moveToIndex(p:Part,i:Int): A
    def setData(kvs:Seq[(Part,Data)]): A

    def addSchemaElt(elt:Elt): A = a.addSchemaElts(Seq(elt))
    def +(elt:Elt) = a.addSchemaElt(elt)
    def ++(elts:Seq[Elt]) = a.addSchemaElts(elts)
    def ++(s:Sch) = a.addSchemaElts(s.generators)


    
    /* Generic methods */

    // def renameSchemaElt(elt:SchemaElt,newName:String) = 
      
    //   a.setSchema(
    //   a.schema.renameElt(elt.id,newName)
    // )
    // def renameSchemaElt(id:UUID,newName:String) = a.setSchema(
    //   a.schema.renameElt(id,newName)
    // ) 
      


    /* Global properties */
    def globalProps: PropMap = a.globalData.getProps()
    def tryGlobalProp(f:Property): Option[f.Value] = a.globalProps.get(f)
    def getGlobalProp(f:Property): f.Value = a.globalProps(f)



    def setGlobalProps(props:PropMap): A = props.pmap.keys.toSeq match
      case Seq() => a
      case f +: rest => 
        a.setGlobalProp(f,props(f)).setGlobalProps(props - f)
        
    def softSetGlobalProps(props:PropMap): A =
      a.setGlobalProps(props ++ globalProps)

    /* Manipulating parts */

    // def getParts(ob:Ob): Seq[Part] = ob match
    //   case ob:ACSetOb => _getParts(ob)
    //   case _ => Seq()
    def allParts() = a.schema.obs
      .flatMap(a.getParts(_))

    def allProps() = a.schema.obs
      .flatMap(a.getProps(_))

    def selectedProps() = a.allProps()
      .filter((_,props) => props.contains(Selected))

    def selected() = a.selectedProps().map(_._1) 

    def hasPart(part:Part) =
        a.getParts(part.ob).contains(part)

    def addParts(ob:Ob,data:Seq[Data | PropMap]): (A,Seq[Part]) = 
      a._addParts(data.map{ _ match
        case props:PropMap => Part(ob) -> PartData[Data](props)
        case d:Data => Part(ob) -> d
        // case (part:Part,props:PropMap) => part -> PartData[Data](props)
        // case (part:Part,d:Data) => part -> d
      })
    
    def addParts(kvs:Seq[(Part,Data | PropMap)]): (A,Seq[Part]) =
      a._addParts(kvs.map((part,data) => data match
        case props:PropMap => part -> PartData[Data](props)
        case d:Data => part -> d  
      ))
 
    def addPart(part:Part,data:Data | PropMap): (A,Part) =
      val (acset,ps) = a.addParts(Seq(part -> data))
      
      (acset,ps.head)


    def addPart(ob:Ob,data:Data | PropMap = PropMap()): (A,Part) =
      val (acset,ps) = a.addParts(ob,Seq(data))
      (acset,ps.head)

    def addParts(ob:Ob,n:Int,data: Data | PropMap = PropMap()): (A,Seq[Part]) =
      a.addParts(ob,Seq.fill(n)(data))

      
    // def remParts(ps:Seq[Part]) = a.remParts(ps.collect{
    //   case p:Part => p
    // })
    def remPart(p:Part) = a.remParts(Seq(p))
    
    /* Setting order & properties */
    def moveToFront(p:Part): A = 
      moveToIndex(p,0)

    def moveToEnd(p:Part): A = 
      moveToIndex(p,a.getParts(p.ob).length - 1)


    /* PartData getters & setters */

    // def getData(p:Part): Data = p match
    //   case p:Part => a._getData(p)
    //   case _ => data()

    def getData(ps:Seq[Part]): Map[Part,Data] =
      ps.map(p => p -> a.getData(p)).toMap

    def getData(ob:Ob): Map[Part,Data] =
      a.getData(a.getParts(ob))
      
    def getDataSeq(ps:Seq[Part]): Seq[(Part,Data)] =
      ps.map(p => p -> a.getData(p))

    def getDataSeq(ob:Ob): Seq[(Part,Data)] =
      a.getDataSeq(a.getParts(ob))
      

    // def setData(kvs:Seq[(Part,Any)]): A =
    //   _setData(kvs.collect{ case (p:Part,d:Data) => (p,d)})

    def setData(p:Part,data:Data): A = setData(Seq(p -> data))


    /* Property getters */
    
    /* Get a family of properties for each part */
    def getProps(part:Part): PropMap =
      a.getData(part).getProps()

    def getProps(ps:Seq[Part]): Map[Part,PropMap] =
      ps.map(p => p -> a.getProps(p)).toMap

    def getProps(ob:Ob): Map[Part,PropMap] =
      a.getProps(a.getParts(ob))

    def getPropSeq(ps:Seq[Part]): Seq[(Part,PropMap)] =
      ps.map(p => p -> a.getProps(p))

    def getPropSeq(ob:Ob): Seq[(Part,PropMap)] =
      a.getPropSeq(a.getParts(ob))

    /* Check if an individual property is defined */
    def hasProp(f:Property,part:Part): Boolean =
      hasPart(part) & getProps(part).contains(f)
    
    
    /* Check if a family of properties is defined */
    def hasProps(fs:Seq[Property],part:Part): Boolean =
      fs.forall(hasProp(_,part))
    def hasProps(props:PropMap,part:Part): Boolean =
      hasProps(props.keySeq,part)

    


    def tryProp(f:Property,part:Part): Option[f.Value] =
      a.getProps(part).get(f)

    def tryProp(f:Property,ps:Seq[Part]): Map[Part,Option[f.Value]] =
      ps.map(p =>
        p -> a.tryProp(f,p)    
      ).toMap

    def tryProp(f:Property,ob:Ob): Map[Part,Option[f.Value]] =
      a.tryProp(f,getParts(ob))

    def collectProp(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
      ps.filter(hasProp(f,_)).map(p =>
        p -> a.getProp(f,p)    
      ).toMap

    def collectProp(f:Property,ob:Ob): Map[Part,f.Value] =
      a.collectProp(f,getParts(ob))


    def getProp(f:Property,p:Part): f.Value =
      tryProp(f,p).get

    def getProp(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
      ps.map(p => p -> a.getProp(f,p)).toMap

    def getProp(f:Property,ob:Ob): Map[Part,f.Value] =
      a.getProp(f,a.getParts(ob))

    def getProp(f:Property,p:Part,default:f.Value): f.Value =
      tryProp(f,p).getOrElse(default)

    def getProp(f:Property,ps:Seq[Part],default:f.Value): Map[Part,f.Value] =
      ps.map(p => p -> a.getProp(f,p,default)).toMap

    def getProp(f:Property,ob:Ob,default:f.Value): Map[Part,f.Value] =
      a.getProp(f,a.getParts(ob),default)


    /* Property setters */

    /* Set a single property on a single part */
    def setProp(f:Property,p:Part,v:f.Value): A = 
      a.setData(p,a.getData(p).setProp(f,v))

    /* Set a single property on many parts */
    def setProp(f:Property,kvs:Seq[(Part,f.Value)]): A =
      a.setData(kvs.map( (part,fval) => 
        part -> a.getData(part).setProp(f,fval)  
      )) 
    
    /* Set a single property on many parts */
    def setProp(f:Property,ps:Seq[Part],v:f.Value): A =
      a.setProp(f,ps.map(_ -> v))
      
    /* Set a single property on many parts */
    def setProp(f:Property,ob:Ob,v:f.Value): A =
      a.setProp(f,a.getParts(ob),v)
      
    /* Set many properties on a single part */
    def setProps(p:Part,props:PropMap): A = 
      a.setData(p,a.getData(p).setProps(props))
    
    /* Set many properties on many parts */
    def setProps(kvs:Seq[(Part,PropMap)]): A = 
      a.setData(kvs.map((part,props) => part ->
        a.getData(part).setProps(props)  
      ))


    /*=========================================*/
    
    /* Set a single property on a single part if unset */
    def softSetProp(f:Property,p:Part,v:f.Value): A = 
      if a.hasProp(f,p) then a else a.setProp(f,p,v)
      
    /* Set a single property on many parts if unset */
    def softSetProp(f:Property,kvs:Seq[(Part,f.Value)]): A =
      a.setProp(f,kvs.filterNot( (p,_) => a.hasProp(f,p) ))
      
    /* Set many properties on a single part if unset */
    def softSetProps(p:Part,props:PropMap): A = 
      a.setProps(p,props ++ a.getProps(p))
      
    /* Set many properties on many parts if unset */
    def softSetProps(kvs:Seq[(Part,PropMap)]): A = 
      a.setProps(kvs.map( (part,props) => 
        part -> (props ++ a.getProps(part))
      ))
    
    def softSetObProps(kvs:Seq[(Ob,PropMap)]): A =
      a.softSetProps(kvs.flatMap((ob,props) =>
        a.getParts(ob).map(_ -> props)  
      ))

    def remProp(f:Property,p:Part): A = 
      a.setData(p,a.getData(p).remProp(f))

    def remProp(f:Property,ps:Seq[Part]): A =
      a.setData(ps.map(p => p -> a.getData(p).remProp(f)))
      
    def remProp(f:Property,ob:Ob): A =
      a.remProp(f,a.getParts(ob))
      

    def remProps(fs:Seq[Property],p:Part): A = 
      a.setProps(p,a.getProps(p) -- fs)

  
  }
}



/** This object contains the constructor method for ACSets and also a collection
  * of wrappers around NestedACSet methods in the `State` monad that allow for a
  * quasi-imperative API for modifying ACSets purely.
  */
object ACSet {


  /** Construct a new ACSet with schema `s` and top-level parts `props` */
  def apply[S:Schema,A:ACSetWithSchema[S]](s: S): A =
    summon[ACSet[A]].fromSchema(s)

}




//=================================================================




  //   /* Generic methods */

  //   /* Global properties */

  //   /* Manipulating parts */


  //   /* PartData getters & setters */



  // // type AACSet[SchOb<:AOb,Data,S<:AbstractSchema[SchOb],A<:AbstractACSet[SchOb,Data,S,A]] = AbstractACSet[SchOb,Data,S,A]

    
  //   /* ACSet */
  //   // type PartType
  //   // type Part <: Part[PartType]
  //   // type PartData
  //   // type SchemaType <: ASchema[SchemaType]

  //   /* Implementation interface */

  //   /* Abstract methods */

  //   /* Checks */

  //   /* Getters */

  //   // def getParts(ptype:PartType): Seq[Part] = _getParts(ptype)

  //   // def getData(ptype:PartType): Map[Part,PartData] = 
  //   //   getData(getParts(ptype)) 
  //   def getData(ob:schema.SchOb): Map[Part,Data] = 
  //     getData(getParts(ob))
  //   def getData[SchOb<:AbstractOb](x:SchOb): Map[Part,Data] = x match
  //     case x:schema.SchOb => getData(x)
  //     case _ =>
  //       println(s"Bad ob $ob in $this")
  //       Map[Part,Data]()
  //   def getData[SchOb<:AbstractOb](ob:SchOb)(implicit eq: =:=[SchOb,schema.SchOb]): Map[Part,Data] =
  //     getData(ob)

  //     case x:schema.SchOb => getData(x)
  //     case _ =>
  //       println(s"Bad ob $ob in $this")
  //       Map[Part,Data]()
    // def allData(): Map[Part,PartData] =
    //   schema.obs.flatMap(getData).toMap

    // def getProps(ptype:PartType): Map[Part,PropMap] =
    //   getProps(getParts(ptype))
    // def allProps(): Map[Part,PropMap] =
    //   getProps(allParts())
      
    // def tryProps(f:Property,ps:Seq[Part]): Map[Part,Option[f.Value]] = 
    //   ps.map(p => p -> tryProp(f,p)).toMap
    // def tryProps(f:Property,ptype:PartType): Map[Part,Option[f.Value]] =
    //   tryProps(f,getParts(ptype))
    // def tryProps(f:Property,ob:schema.SchOb): Map[Part,Option[f.Value]] =
    //   tryProps(f,getParts(ob))
    // def tryProps(f:schema.Hom): Map[Part,Option[f.Value]] =
    //   tryProps(f,f.dom)
    // @targetName("tryAttrProps")
    // def tryProps(f:schema.Attr): Map[Part,Option[f.Value]] =
    //   tryProps(f,f.dom)
    


    // def collectProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
    //   ps.collect{ case p if hasProp(f,p) => p -> getProp(f,p) }.toMap
    // def collectProps(f:Property,ptype:PartType): Map[Part,f.Value] =
    //   collectProps(f,getParts(ptype))
    // def collectProps(f:Property,ob:schema.SchOb): Map[Part,f.Value] =
    //   collectProps(f,getParts(ob))

    // /** Mutation **/
    // /* Adding parts */
    // def addPart(ob:schema.SchOb,data:Seq[PartData]): (A,Part) =
    //   addPart(partType(ob),data)
    // def addParts(ptype:PartType,n:Int,data:PartData): (A,Seq[Part]) =
    //   addParts(ptype,Seq.fill(n)(data))
    // def addParts(ob:schema.SchOb,n:Int,data:PartData): (A,Seq[Part]) =
    //   addParts(partType(ob),n,data)
    // def addParts(ptype:PartType,n:Int,props:PropMap): (A,Seq[Part]) =
    //   addParts(ptype,n,partData(props))
    // def addParts(ob:schema.SchOb,n:Int,props:PropMap): (A,Seq[Part]) =
    //   addParts(partType(ob),n,partData(props))
    // def addParts(kvs:(PartType,PropMap) | (schema.SchOb,PropMap)
    //   | (PartType,PartData) | (schema.SchOb,PartData)*): (A,Seq[Part]) = 
    //   val cleankvs = kvs.map(_ match
    //     case (tp:PartType,data:PartData) => (tp,data)
    //     case (ob:schema.SchOb,data:PartData) => (partType(ob),data)
    //     case (tp:PartType,props:PropMap) => (tp,partData(props))
    //     case (ob:schema.SchOb,props:PropMap) => (partType(ob),partData(props))
    //   )
    //   cleankvs match
    //     case Seq() => (self,Seq())
    //     case (tp,data) +: tail =>  
    //       val (next,first) = addPart(tp,data)
    //       val (last,rest) = addParts(tail:_*)
    //       (last,first +: rest)

    // /* Removing parts */

    // def remPart(p:Part): A = remPart(this,p)  
    // def remParts(a:A,ps:Seq[a.Part]): A = ps match
    //   case Seq() => this
    //   case head +: tail => remPart(a,head).remParts(a,tail)
    // def remParts(ps:Seq[Part]): A = remParts(this,ps)
    // def remParts(ps:Seq[Part],pred:Part => Boolean): A =
    //   remParts(ps.filter(pred))
    // def remParts(ptype:PartType,pred:Part => Boolean): A =
    //   remParts(getParts(ptype),pred)
    // def remParts(ob:schema.SchOb,pred:Part => Boolean): A =
    //   remParts(getParts(ob),pred)

    
    // def issimple(data:PartData) =
    //   partData(getDataProps(data)) == data
    
    // def setData(part:Part,data:PartData): A = ?






sealed trait ACSetMsg[A:ACSet] extends AtomicMessage[A]

case class AddPartMsg[A:ACSet](ob:Ob,props:PropMap = PropMap(),idOpt:Option[UUID] = None) extends ACSetMsg[A]:
  def execute(a:A) = idOpt match
    case Some(id) => a.addPart(Part(id,ob),props)._1
    case None => a.addPart(ob,props)._1

object AddPartMsg:
  def apply[A:ACSet](ob:Ob,props:PropMap,id:UUID) =
    new AddPartMsg[A](ob,props,Some(id))

case class RemovePartMsg[A:ACSet](part:Part) extends ACSetMsg[A]:
  def execute(a:A) = a.remPart(part)

case class ChangePropMsg[A:ACSet](part:Part,pval:PropChange[_]) extends ACSetMsg[A]:
  def execute(a:A) = pval match
    case PropChange(f,_,Some(v)) => a.setProp(f,part,v)
    case PropChange(f,_,None) => a.remProp(f,part)

object ChangePropMsg:
  def apply[A:ACSet](part:Part,f:Property,oldVal:f.Value,newVal:f.Value) = new ChangePropMsg(part,PropChange(f,oldVal,newVal))

case class SetPropsMsg[A:ACSet](part:Part,props:PropMap) extends ACSetMsg[A]:
  def execute(a:A) = a.setProps(part,props)

case class SetPropMsg[A:ACSet](part:Part,pval:PropVal[_]) extends ACSetMsg[A]:
  def execute(a:A) = pval match
    case PropVal(f,Some(v)) => a.setProp(f,part,v)
    case PropVal(f,None) => a.remProp(f,part)

object SetPropMsg:
  def apply[A:ACSet](part:Part,f:Property,newVal:f.Value) = new SetPropMsg(part,PropVal(f,newVal))


case class RenameOb[A:ACSet](id:UUID,newOb:Ob) extends ACSetMsg[A]:
  def execute(a:A) = 
    a.replaceSchemaElts(Seq(id -> newOb))
    // pval match
    // case PropVal(f,Some(v)) => a.setProp(f,part,v)
    // case PropVal(f,None) => a.remProp(f,part)

object RenameOb:
  def apply[A:ACSet](id:UUID,ob:Ob) = new RenameOb[A](id,ob)



// case class MsgSeq[A](override val msgs:Seq[ACSetMsg[A]]) extends ACSetMsg[A]:
//   def execute(a:A) = msgs.foldLeft(a)((acset,msg) => msg.execute(acset))
  
// object MsgSeq:
//   def apply[A:ACSet]() = new MsgSeq[A](Seq())
// case class RemovePropMsg[A:ACSet](prop:Property,part:Part) extends ACSetMsg[A]:
//   def execute(a:A) = a.remProp(prop,part)









// trait AOb



// trait Schema[SchOb<:AOb]:
//   def obs: Seq[SchOb]

// trait ACSet[SchOb<:AOb,Data:PartData](val schema:Schema[SchOb]):
//   def parts(x:SchOb): Seq[Part]
//   def getData(p:Part): Data  

// case class Part(ob:SchOb,i:Int)

// case class SimpleOb(name:String) extends AOb
// case class SimpleSchema(
//   obs: Seq[SimpleOb]
// ) extends Schema[SimpleOb]

// case class SimpleACSet[SchOb<:AOb,Data:PartData](
//   override val schema:Schema[SchOb],
//   partStore: Map[SchOb,Map[Part,Data]]
// ) extends ACSet[SchOb,Data](schema):
//   def parts(x:SchOb) = partStore(x).keys.toSeq
//   def getData(p:Part): Data = partStore(p.ob)(p)




// case class SimpleACSet()

// val simpleACSetIsACSet: ACSet[SimpleACSet] = new ACSet[SimpleACSet] {}
