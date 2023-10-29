package semagrams.acsets.simple

import semagrams._
import acsets._
import acsets.abstr._
import semagrams.util._


import upickle.default._









/* Parts */




type PartStore = Map[Ob,PartSet]

object PartStore:
  def apply() = Map[Ob,PartSet]().withDefaultValue(PartSet())





case class SimpleACSet[+S:Schema](
  schema:S,
  globalData: PropMap,
  partStore: PartStore
):
  override def toString = "SimpleACSet(" + "\n" + 
    schema.obs.map(ob => "\t" + ob.toString + "->\n" + 
      partStore(ob).propStore.toSeq.map((id,props) =>
        s"\t\t$props\n"
      ).mkString
    ).mkString + ")"

object SimpleACSet:

  import Table.TableDef

  def schemaElts(defn:ACSetEltDef): Seq[SchemaElt] = defn match
    case (t:TableDef,rows) => Seq(Table(t))
    
    
  

  type ACSetEltDef = PartDef
  type PartDef = (TableDef,Int) | (TableDef,Seq[PropMap])




  // def apply():SimpleACSet[SimpleSchema] = apply[SimpleSchema]()(simpleSchemaIsSchema)
  // def apply[S:Schema]():SimpleACSet[S] = apply(Schema[S]())


  // def apply(): SimpleACSet[SimpleSchema] = 
  //   implicit val schDef: Schema[SimpleSchema] = simpleSchemaIsSchema
  //   SimpleACSet(SimpleSchema())
  // def apply[S:Schema](sch:S): SimpleACSet[S] = 
  //   SimpleACSet(sch,PropMap())
  // def apply[S:Schema](sch:S,props:PropMap): SimpleACSet[S] = 
  //   SimpleACSet(sch,props,PartStore())
  def apply[S:Schema](defns:ACSetEltDef*): SimpleACSet[S] = defns match
    case Seq() => apply(Schema[S]())
    case defns => 
      val elts: Seq[SchemaElt] = defns.flatMap(schemaElts)
      val sch = Schema[S]().addElts(elts)
      apply(sch,defns:_*)


  def apply[S:Schema](sch:S,defns:ACSetEltDef*): SimpleACSet[S] =
    implicit val acsetDef: ACSet[SimpleACSet[S]] = simpleACSetIsACSet[S]


    defns.foldLeft(
      new SimpleACSet[S](
        Schema[S](),
        PropMap(),
        PartStore()        
      )
    )( (acset,defn) => 
      defn match
      case (t:TableDef,rows) => rows match
        case n:Int =>
          acset.addParts(Table(t),n)._1
        case ps:Seq[PropMap] =>
          acset.addParts(Table(t),ps)._1
        
      
    

    )
    
  def simpleACSetIsACSet[S:Schema](s:S): ACSetWithSchemaAndData[S,PropMap][SimpleACSet[S]] = 
    simpleACSetIsACSet[S]


  def simpleACSetIsACSet[S](
    using sis:Schema[S],
    // did:PartData[D]
  ):ACSetWithSchemaAndData[S,PropMap][SimpleACSet[S]] =
    new ACSet[SimpleACSet[S]] {

      val name = "SimpleACSet - " + sis.name

  
      type Data = PropMap
      implicit val dataIsPartData: PartData[PropMap] =
        PartData.propsAreData
      // implicit val dataIsPartData: PartData[Data] = 
        

      type Sch = S
      implicit val schemaIsSchema = 
        sis
      def fromSchema(s: Sch): SimpleACSet[S] = 
        SimpleACSet(s)(sis)
    
      extension (a:SimpleACSet[S])
        def schema = a.schema
        def globalData = a.globalData
        def setGlobalProp(f:Property,v:f.Value) = a.copy(
          globalData = a.globalData + (f,v)
        )
        def remGlobalProp(f:Property) = a.copy(
          globalData = a.globalData - f
        )
        def addSchemaElts(elts:Seq[Elt]) = a.copy(
          schema = a.schema ++ elts
        )

        def getParts(ob:Ob): Seq[Part] = 
          a.partStore(ob).ids.map(Part(_,ob))

        def getData(p:Part): PropMap = 
          a.partStore.get(p.ob).flatMap(
            _.propStore.get(p.id)
          ).getOrElse(PropMap())

        def _addParts(kvs:Seq[(Part,PropMap)]): (SimpleACSet[S],Seq[Part]) =
          kvs.groupBy((part,_) => part.ob)
            .foldLeft((a,Seq[Part]())){ case  ((acset,newParts),(ob,partData)) =>
              val (nextStore,nextIds) = a.partStore(ob).addParts(
                partData.map( (part,data) => (part.id,data))
              ) 
              val nextACSet = a.copy(
                schema = if schema.contains(ob) then schema else schema + ob,
                partStore = acset.partStore + (ob -> nextStore)
              )
              (nextACSet,newParts ++ nextIds.map(Part(_,ob)))
            }
          
          
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


}
