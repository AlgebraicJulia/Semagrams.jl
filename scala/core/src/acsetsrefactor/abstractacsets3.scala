// package semagrams.acsets2.abstr

// import semagrams._
// import semagrams.acsets._

// import upickle.default._
// // import scala.annotation.targetName
// // import javax.xml.validation.Schema



// /* General categorical entities (e.g., `X`, `Arrow`) */
// trait AbstractElt:
//   def generators: Seq[GenElt]
//   def label:String
//   override def toString(): String = label
// type AElt = AbstractElt



// trait AbstractOb extends AElt with EntityType
// type AOb = AbstractOb


// trait AbstractArrow[X<:AOb] extends AElt:
//   def dom: X    
//   def codom: AOb
//   def path: Seq[GenArrow[_>:X]]
// type AArrow[X<:AOb] = AbstractArrow[X]


// // trait AbstractTable extends AbstractOb
// // type ATable = AbstractTable

// trait AbstractType extends AbstractOb:
//   type ValueType
//   def rw: ReadWriter[ValueType]
// type AType = AbstractType


// trait AbstractHom[X<:AOb,Y<:AOb] extends AbstractArrow[X]:
//   type Value = Part
//   def rw(using yRW:ReadWriter[Y]): ReadWriter[Part] = Part.rw[Y]
//   // val dom: X
//   val codom: Y
// type AHom[X<:AOb,Y<:AOb] = AbstractHom[X,Y]
 
// trait AbstractAttr[X<:AOb] extends AbstractArrow[X]:
//   // type Value = T
//   // val rw = readwriter[T]
//   val dom: X
//   // val codom: Y

// // type AAttr[X<:AOb,Y[_]<:AType[_]] = AbstractAttr[X,Y[_]]

// /* Generating categorical entities */
// trait GenElt:
//   def generators = Seq(this)
//   def name: String
//   def label = name

// // trait GenOb extends AOb with GenElt:
// //   val rw: ReadWriter[Part]
// // trait GenType[T:ReadWriter] extends AType[T] with GenElt:
// //   val rw: ReadWriter[T] = readwriter[T]
 
// trait GenArrow[X<:AOb] extends AbstractArrow[X] with GenElt:
//   def path = Seq(this)
//   override def generators = dom.generators ++ codom.generators :+ this

// trait GenHom[X<:AOb,Y<:AOb:ReadWriter] extends GenArrow[X] 
//   with AbstractHom[X,Y] with Property:
//   type Value = Part
//   val rw: ReadWriter[Value] = Part.rw[Y]

// trait GenAttr[X<:AOb] extends GenArrow[X]
//   with AbstractAttr[X]

  
// case class Part(ob:X,id:Id) 
//   extends Entity:
//   def idNum = id.id
//   val ty = ob

//   /** Transform to an name that is usable in tikz */
//   def tikzName: String = ob.label + idNum.toString


// object Part:
//   // def apply[X<:AOb:ReadWriter](x:X,id:Id) = new
//   def rw[X<:AOb:ReadWriter]: ReadWriter[Part] = 
//     readwriter[(String,Int)].bimap[Part](
//       part => (write(part.ob),part.id.id),
//       (obstr,i) => Part(read[X](obstr),Id(i))
//     )

// trait AbstractSchema[X<:AOb]:
//   // type SchTable <: AOb & GenElt
//   // type SchHom <: GenArrow[SchTable,SchTable]
//   // type SchAttr <: GenArrow[SchTable,_]
//   // type SchType <: AType[_] & GenElt
//   // type SchHom <: GenHom[SchTable,SchTable]
//   // type SchAttr <: [T] =>> GenAttr[SchTable,SchType]
  
//   // type Part = Part


//   def obs: Seq[X]
//   def homs: Seq[AHom[X,X]]
//   def attrs: Seq[AbstractAttr[X]]
//   def globalProps: Seq[Property] = Seq()

//   def generators: Seq[GenElt] = (obs ++ homs ++ attrs).flatMap(_.generators)

//   override def toString = "Schema(" + 
//     (if obs.isEmpty then "" else 
//       "\n  X:   " + obs.mkString(", ")
//     ) + (if homs.isEmpty then "" else 
//       "\n  Hom:  " + homs.mkString(", ")
//     ) + (if attrs.isEmpty then "" else 
//       "\n  Observable: " + attrs.mkString(", ")
//     ) + (if globalProps.isEmpty then "" else 
//       "\n  Prop: " + globalProps.mkString(", ")
//     ) + (if (generators ++ globalProps).isEmpty 
//       then ")\n" else "\n)\n"
//     )
// type ASchema[X<:AOb] = AbstractSchema[X]
  

// trait DynSchema[X<:AOb,S<:DynSchema[X,S]] extends AbstractSchema[X]:
//   def addElts(elts:Seq[GenElt]): S
//   def addProps(prop:Seq[Property]): S

//   def +(elt:AElt) = addElts(elt.generators)
//   def +(prop:Property) = addProps(Seq(prop))

//   // def ++(stuff:Seq[AElt | Property]): S = addElts(
//   //   stuff.collect{ case x:AElt => x}
//   //     .flatMap(_.generators)
//   //   ).addProps(stuff.collect{ case p:Property => p})

//   // def ++(s:S): S = ++(s.generators) ++ s.props




// trait PartData[D] {
//   def toData(props:PropMap): D
//   def newData(): D = toData(PropMap()) 
//   extension (d:D)
//     /* Implementation API */
//     def getProps(): PropMap
//     def setProp(f:Property,v:f.Value): D
//     def remProp(f:Property): D
//     /* Generic methods */
//     def setProps(props:PropMap): D =
//       props.pmap.keys.toSeq match
//         case Seq() => d
//         case f +: rest => d.setProp(f,props(f)).setProps(props - f)
// }


// object PartData:
//   implicit val propsAreData:PartData[PropMap] = new PartData[PropMap] {
//     def toData(props:PropMap) = props
//     extension (props:PropMap)
//       def getProps() = props
//       def setProp(f:Property,v:f.Value) = props + (f,v)
//       def remProp(f:Property) = props - f
//   }







// trait AbstractACSet[
//   X<:AOb,
//   D:PartData,
//   S <:AbstractSchema[X],
//   A <:AbstractACSet[X,D,S,A]
// ](val schema:S) { self:A =>

//   val partData = summon[PartData[D]]
//   // val schema: AbstractSchema
//   // import schema._
//   // // type X = schema.SchTable
//   // type Part = Part


//   /* Implementation API */

//   /* Global properties */
//   def globalData: D

//   /* Manipulating parts */
//   def _getParts(ob:X): Seq[Part]


  
//   def _getData(part:Part): D



//   /* Generic methods */

//   /* Global properties */

//   def globalProps: PropMap = globalData.getProps()
//   def tryGlobalProp(f:Property): Option[f.Value] = globalProps.get(f)
//   def getGlobalProp(f:Property): f.Value = globalProps(f)


//   /* Manipulating parts */

//   def getParts(ob:AOb): Seq[Part] = ob match
//     case ob:X => _getParts(ob)
//     case _ => Seq()

//   def hasPart(part:Part) = 
//     getParts(part.ob).contains(part)

//   /* PartData getters & setters */

//   def getData(p:Part): D = p match
//     case p:Part => _getData(p)
//     case _ => partData.toData(PropMap())

//   def getData(ps:Seq[Part]): Map[Part,D] =
//     ps.map(p => p -> getData(p)).toMap

//   def getData(ob:AOb): Map[Part,D] =
//     getData(getParts(ob))


//   /* Property getters & setters */
  
//   /* Get a family of properties for each part */
//   def getProps(part:Part): PropMap =
//     getData(part).getProps()
//   def getProps(ps:Seq[Part]): Map[Part,PropMap] =
//     ps.map(p => p -> getProps(p)).toMap
//   def getProps(ob:AbstractOb): Map[Part,PropMap] =
//     getProps(getParts(ob))

//   /* Check if an individual property is defined */
//   def hasProp(f:Property,part:Part) =
//     hasPart(part) & getProps(part).contains(f)
//   // def hasProp(f:Property,ps:Seq[Part]): Seq[Boolean] =
//   //   ps.map(hasProp(f,_))
//   // def hasProp(f:Property,ob:AOb): Seq[Boolean] =
//   //   hasProp(f,getParts(ob))
//   /* Check if a collection of properties is defined */
//   def hasProps(fs:Seq[Property],part:Part) =
//     fs.forall(hasProp(_,part))
//   def hasProps(props:PropMap,part:Part): Boolean =
//     hasProps(props.keySeq,part)

  


//   def tryProp(f:Property,part:Part): Option[f.Value] =
//     getProps(part).get(f)
//   def collectProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
//     ps.collect{ case p:Part if hasProp(f,p) =>
//       p -> getProp(f,p)  
//     }.toMap
//   def collectProps(f:Property,ob:AOb): Map[Part,f.Value] =
//     collectProps(f,getParts(ob))

//   def getProp(f:Property,part:Part): f.Value =
//     tryProp(f,part).get
//   def getProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
//     ps.map(p => p -> getProp(f,p)).toMap
//   def getProps(f:Property,ob:AbstractOb): Map[Part,f.Value] =
//     getProps(f,getParts(ob))



// //   // def getProp(f:Property,part:Part): f.Value =
// //   //   tryProp(f,part).get
// //   // def getProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
// //   //   ps.map(p => p -> getProp(f,p)).toMap
// //   // def getProps(f:Property,ptype:PartType): Map[Part,f.Value] =
// //   //   getProps(f,getParts(ptype))
// //   // def getProps(f:Property,ob:schema.X): Map[Part,f.Value] =
// //   //   getProps(f,getParts(ob))

  
//   // def hasProp(f:Arrow): Seq[Boolean] = f match
//   //   case f:Hom => hasProp(f,f.dom)
//   //   case f:Attr => hasProp(f,f.dom)
  
//   def allParts() = schema.obs.flatMap(getParts)


//   def setGlobalProp(f:Property,v:f.Value): A
//   def remGlobalProp(f:Property): A
//   def _addParts(ob:X,data:Seq[D]): (A,Seq[Part])  
//   def _remPart(p:Part): A
//   def _moveToIndex(p:Part,i:Int): A
//   def _setData(kvs:Seq[(Part,D)]): A

//   /* Generic methods */

//   /* Global properties */


//   def setGlobalProps(props:PropMap): A = props.pmap.keys.toSeq match
//     case Seq() => this
//     case f +: rest => 
//       val nxt = setGlobalProp(f,props(f))
//       val last = nxt.setGlobalProps(props - f)
//       last

//   def softSetGlobalProps(props:PropMap): A =
//     setGlobalProps(props ++ globalProps)

//   /* Manipulating parts */

//   def addParts(ob:AOb,data:Seq[D | PropMap]): (A,Seq[Part]) = ob match
//     case ob:X => _addParts(ob,data.map{
//       case d:D => d
//       case props:PropMap => partData.toData(props)
//     })

//   def addPart(ob:AOb,data:D | PropMap = PropMap()): (A,Part) =
//     val (acset,ps) = addParts(ob,Seq(data))
//     println(ps)
//     (acset,ps.head)

//   def addParts(ob:AOb,n:Int,data: D | PropMap = PropMap()): (A,Seq[Part]) =
//     addParts(ob,Seq.fill(n)(data))

//   def remPart(p:Part) = p match
//     case p:Part => _remPart(p)
//     case _ => this

//   def remParts(ps:Seq[Part]) = ps match
//     case Seq() => this
//     case head +: tail => head match
//       case p:Part => _remPart(p)
//       case _ => this
    
  
//   def moveToIndex(p:Part, i:Int) = p match
//     case p:Part => _moveToIndex(p,i)
//     case _ => this


//   /* PartData getters & setters */

//   def setData(kvs:Seq[(Part,Any)]) =
//     _setData(kvs.collect{ case (p:Part,d:D) => (p,d)})

//   def setData(p:Part,data:D): A = setData(Seq(p -> data))


//   /* Property getters & setters */

//   /* Set a single property on a single part */
//   def setProp(f:Property,p:Part,v:f.Value): A = 
//     setData(p,getData(p).setProp(f,v))

//   /* Set a single property on many parts */
//   def setProps(f:Property,kvs:Seq[(Part,f.Value)]): A =
//     setData(kvs.map((part,fval) => part ->
//       getData(part).setProp(f,fval)  
//     )) 
  
//   /* Set many properties on a single part */
//   def setProps(p:Part,props:PropMap): A = 
//     setData(p,getData(p).setProps(props))
  
//   /* Set many properties on many parts */
//   def setProps(kvs:Seq[(Part,PropMap)]): A = 
//     setData(kvs.map((part,props) => part ->
//       getData(part).setProps(props)  
//     ))


//   /*=========================================*/
  
//   /* Set a single property on a single part if unset */
//   def softSetProp(f:Property,p:Part,v:f.Value): A = 
//     if hasProp(f,p) then this else setProp(f,p,v)
    
//   /* Set a single property on many parts if unset */
//   def softSetProps(f:Property,kvs:Seq[(Part,f.Value)]): A =
//     setProps(f,kvs.filter( (p,_) => hasProp(f,p) ))
    
//   /* Set many properties on a single part if unset */
//   def softSetProps(p:Part,props:PropMap): A = 
//     setProps(p,props ++ getProps(p))
    
//   /* Set many properties on many parts if unset */
//   def softSetProps(kvs:Seq[(Part,PropMap)]): A = 
//     setProps(kvs.map((part,props) => part ->
//       (props ++ getProps(part))
//     ))
  
//   def softSetObProps(kvs:Seq[(AOb,PropMap)]): A =
//     softSetProps(kvs.flatMap((ob,props) =>
//       getParts(ob).map(_ -> props)  
//     ))
  
// }
// type AACSet[X<:AOb,D,S<:AbstractSchema[X],A<:AbstractACSet[X,D,S,A]] = AbstractACSet[X,D,S,A]

  
//   // /* ACSet */
//   // type PartType
//   // type Part <: Part[PartType]
//   // type PartData
//   // type SchemaType <: ASchema[SchemaType]

// //   /* Implementation interface */

// //   /* Abstract methods */

// //   /* Checks */

// //   /* Getters */

// //   // def getParts(ptype:PartType): Seq[Part] = _getParts(ptype)

// //   // def getData(ptype:PartType): Map[Part,PartData] = 
// //   //   getData(getParts(ptype)) 
//   // def getData(ob:schema.X): Map[Part,D] = 
//   //   getData(getParts(ob))
//   // def getData[X<:AbstractOb](x:X): Map[Part,D] = x match
//   //   case x:schema.X => getData(x)
//   //   case _ =>
//   //     println(s"Bad ob $ob in $this")
//   //     Map[Part,D]()
//   // def getData[X<:AbstractOb](ob:X)(implicit eq: =:=[X,schema.X]): Map[Part,D] =
//   //   getData(ob)

//     // case x:schema.X => getData(x)
//     // case _ =>
//     //   println(s"Bad ob $ob in $this")
//     //   Map[Part,D]()
// //   // def allData(): Map[Part,PartData] =
// //   //   schema.obs.flatMap(getData).toMap

// //   // def getProps(ptype:PartType): Map[Part,PropMap] =
// //   //   getProps(getParts(ptype))
// //   // def allProps(): Map[Part,PropMap] =
// //   //   getProps(allParts())
    
// //   // def tryProps(f:Property,ps:Seq[Part]): Map[Part,Option[f.Value]] = 
// //   //   ps.map(p => p -> tryProp(f,p)).toMap
// //   // def tryProps(f:Property,ptype:PartType): Map[Part,Option[f.Value]] =
// //   //   tryProps(f,getParts(ptype))
// //   // def tryProps(f:Property,ob:schema.X): Map[Part,Option[f.Value]] =
// //   //   tryProps(f,getParts(ob))
// //   // def tryProps(f:schema.Hom): Map[Part,Option[f.Value]] =
// //   //   tryProps(f,f.dom)
// //   // @targetName("tryAttrProps")
// //   // def tryProps(f:schema.Attr): Map[Part,Option[f.Value]] =
// //   //   tryProps(f,f.dom)
  


// //   // def collectProps(f:Property,ps:Seq[Part]): Map[Part,f.Value] =
// //   //   ps.collect{ case p if hasProp(f,p) => p -> getProp(f,p) }.toMap
// //   // def collectProps(f:Property,ptype:PartType): Map[Part,f.Value] =
// //   //   collectProps(f,getParts(ptype))
// //   // def collectProps(f:Property,ob:schema.X): Map[Part,f.Value] =
// //   //   collectProps(f,getParts(ob))

// //   // /** Mutation **/
// //   // /* Adding parts */
//   // def addPart(ob:schema.X,data:Seq[PartData]): (A,Part) =
//   //   addPart(partType(ob),data)
// //   // def addParts(ptype:PartType,n:Int,data:PartData): (A,Seq[Part]) =
// //   //   addParts(ptype,Seq.fill(n)(data))
// //   // def addParts(ob:schema.X,n:Int,data:PartData): (A,Seq[Part]) =
// //   //   addParts(partType(ob),n,data)
// //   // def addParts(ptype:PartType,n:Int,props:PropMap): (A,Seq[Part]) =
// //   //   addParts(ptype,n,partData(props))
// //   // def addParts(ob:schema.X,n:Int,props:PropMap): (A,Seq[Part]) =
// //   //   addParts(partType(ob),n,partData(props))
// //   // def addParts(kvs:(PartType,PropMap) | (schema.X,PropMap)
// //   //   | (PartType,PartData) | (schema.X,PartData)*): (A,Seq[Part]) = 
// //   //   val cleankvs = kvs.map(_ match
// //   //     case (tp:PartType,data:PartData) => (tp,data)
// //   //     case (ob:schema.X,data:PartData) => (partType(ob),data)
// //   //     case (tp:PartType,props:PropMap) => (tp,partData(props))
// //   //     case (ob:schema.X,props:PropMap) => (partType(ob),partData(props))
// //   //   )
// //   //   cleankvs match
// //   //     case Seq() => (self,Seq())
// //   //     case (tp,data) +: tail =>  
// //   //       val (next,first) = addPart(tp,data)
// //   //       val (last,rest) = addParts(tail:_*)
// //   //       (last,first +: rest)

// //   // /* Removing parts */

// //   // def remPart(p:Part): A = remPart(this,p)  
// //   // def remParts(a:A,ps:Seq[a.Part]): A = ps match
// //   //   case Seq() => this
// //   //   case head +: tail => remPart(a,head).remParts(a,tail)
// //   // def remParts(ps:Seq[Part]): A = remParts(this,ps)
// //   // def remParts(ps:Seq[Part],pred:Part => Boolean): A =
// //   //   remParts(ps.filter(pred))
// //   // def remParts(ptype:PartType,pred:Part => Boolean): A =
// //   //   remParts(getParts(ptype),pred)
// //   // def remParts(ob:schema.X,pred:Part => Boolean): A =
// //   //   remParts(getParts(ob),pred)

// //   // /* Setting order & properties */
// //   // def moveToFront(p:Part): A = moveToIndex(p,0)
  
// //   // def issimple(data:PartData) =
// //   //   partData(getDataProps(data)) == data
  
// //   def setData(part:Part,data:PartData): A = 
// //     setData(this,part,data)
// //   // def setData(a:A,kvs:Seq[(a.Part,a.PartData)]): A = kvs match
// //   //   case Seq() => this
// //   //   case (part,data) +: tail =>
// //   //     setData(a,part,data).setData(a,tail)
// //   // def setData(kvs:Seq[(Part,PartData)]): A =
// //   //   setData(this,kvs)
// //   // def setData(ps:Seq[Part],props:PartData): A =
// //   //   setData(ps.map(_ -> props))  

// //   def setDataProps(f:Property,kvs:Seq[(PartData,f.Value)]): Seq[PartData] = 
// //     kvs.map((data,fval) => setDataProp(f,data,fval))
// //   def softSetDataProps(f:Property,kvs:Seq[(PartData,f.Value)]): Seq[PartData] = 
// //     kvs.map((data,fval) => softSetDataProp(f,data,fval))
// //   def remDataProp(f:Property,ds:Seq[PartData]): Seq[PartData] =
// //     ds.map(remDataProp(f,_))
// //   def remDataProps(fs:Seq[Property],ds:Seq[PartData]): Seq[PartData] =
// //     ds.map(remDataProps(fs,_))
// //   def remDataProps(props:PropMap,ds:Seq[PartData]): Seq[PartData] =
// //     ds.map(remDataProps(props.keySeq,_))
  
//   // def setProp(f:Property,p:Part,v:f.Value): A =
//   //   setData(p,getData(p).setProp(f,v))
//   // def setProps(f:Property,kvs:Seq[(Part,f.Value)]): A = 
//   //   setData(kvs.map((p,v) => p -> setDataProp(f,getData(p),v)))
//   // def setProps(p:Part,props:PropMap): A =
//   //   setData(p,setDataProps(getData(p),props))
//   // def setProps(ps:Seq[Part],props:PropMap): A = 
//   //   setData(ps.map(p => p -> getData(p).setProps(props)))
//   // def setProps(ptype:PartType,props:PropMap): A = 
//   //   setProps(getParts(ptype),props)
//   // def setProps(ob:schema.X,props:PropMap): A = 
//   //   setProps(getParts(ob),props)
   
//   // def setProp(p:Part,f:Property,v:f.Value) =
//   //   setProp(f,p,v)
      
  
//   // def softSetProp(f:Property,p:Part,v:f.Value): A = 
//   //   setData(p,softSetDataProp(f,getData(p),v))
//   // def softSetProps(f:Property,kvs:Seq[(Part,f.Value)]): A =
//   //   setData(kvs.map((p,v) => p -> softSetDataProp(f,getData(p),v)))
//   // def softSetProps(p:Part,props:PropMap): A =
//   //   setData(p,setDataProps(getData(p),props))
//   // def softSetProps(ps:Seq[Part],props:PropMap): A =
//   //   setData(ps.map(p => p -> setDataProps(getData(p),props)))
//   // def softSetProps(ptype:PartType,props:PropMap): A =
//   //   softSetProps(getParts(ptype),props)
//   // def softSetProps(ob:schema.X,props:PropMap): A = 
//   //   softSetProps(getParts(ob),props)


//   // def remProp(f:Property,p:Part): A = 
//   //   setData(p,remDataProp(f,getData(p)))
//   // def remProps(fs:Seq[Property],p:Part): A = 
//   //   setData(p,remDataProps(fs,getData(p)))
//   // def remProps(props:PropMap,p:Part): A =
//   //   setData(p,remDataProps(props,getData(p)))

//   // def remProp(f:Property,ps:Seq[Part]): A =
//   //   setData(ps.map(p => p -> remDataProp(f,getData(p))))
//   // def remProps(fs:Seq[Property],ps:Seq[Part]): A =    
//   //   setData(ps.map(p => p -> remDataProps(fs,getData(p))))
//   // def remProps(props:PropMap,ps:Seq[Part]): A =
//   //   setData(ps.map(p => p -> remDataProps(props,getData(p))))

//   // def resetProps(part:Part,props:PropMap): A =
//   //   setData(part,resetDataProps(getData(part),props))





// //   def softSetProp(f:Property,v:f.Value): D =
// //     if getProps().contains(f) then data else setProp(f,data,v)
// //   def softSetProps(props:PropMap): D =
// //     props.pmap.keys.toSeq match
// //       case Seq() => self
// //       case f +: rest => softSetProp(f,props(f)).softSetProps(props - f)
// //   def resetProps(props:PropMap): D =
// //     val clear = remProps(getProps())
// //     clear.setProps(props)
  
// //   def remProps(fs:Seq[Property]): D = fs match
// //     case Seq() => self
// //     case f +: rest => remProp(f).remProps(rest)
// //   def remProps(props:PropMap): D =
// //     remProps(props.keySeq)

 








// // sealed trait ACSetMsg extends Message[AbstractACSet[_]]

// // case class AddPartMsg[X<:AOb](ob:X,props:PropMap = PropMap()) extends ACSetMsg {
// //   def execute(a:AbstractACSet[_]) = ob match
// //     case ob:a.schema.X => a.addPart(ob,props)._1
// //     case _ => 
// //       println(s"unknown ob $ob")
// //       a
// // }

//   // case class RemovePartMsg[S<:ACat[S],A<:AbstractACSet[S,A]](part:Part) extends Message[A] {
//   //   def execute(a:A) = part match
//   //     case part:a.Part => a.remPart(part)
//   //     case _ =>  a
//   // }   

//   // case class SetSubpartMsg[S<:ACat[S],A<:AbstractACSet[S,A]](part:Part,prop:Property)(v:prop.Value) extends Message[A] {
//   //   def execute(a:A) = part match
//   //     case part:a.Part => a.setProp(part,prop,v)
//   //     case _ => a
//   // }

//   // case class RemoveSubpartMsg[S<:ACat[S],A<:AbstractACSet[S,A]](part:Part,prop:Property) extends Message[A] {
//   //   def execute(a:A) = part match
//   //     case part:a.Part => a.remSubpart(part,prop)
//   //     case _ => a
//   // }




