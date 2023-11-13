// package semagrams.graphs


// import semagrams._
// import semagrams.acsets._
// // import semagrams.acsets.abstr._
// import semagrams.util._

// import semagrams.acsets.abstr._

// import upickle.default._




// enum SchObs(val name:String) extends Ob with Generator
//   derives ReadWriter:
//   case TableOb extends SchObs("TableOb")
//   case ValTypeOb extends SchObs("ValTypeOb")
//   case FKeyOb extends SchObs("FKeyOb")
//   case ColumnOb extends SchObs("ColumnOb")
//   val id = UUID("SchObs")
// export SchObs._


// enum SchHoms(val name:String,val dom:SchObs,val codom:SchObs) 
//   extends GenHom[SchObs]:
//   case FKeySrc extends SchHoms("FKeySrc",FKeyOb,TableOb)
//   case FKeyTgt extends SchHoms("FKeyTgt",FKeyOb,TableOb)
//   case ColumnSrc extends SchHoms("ColumnSrc",FKeyOb,TableOb)
//   case ColumnTgt extends SchHoms("ColumnTgt",FKeyOb,ValTypeOb)
//   val id = UUID("SchHoms")
// export SchHoms._

// // enum SchColumns(val name:String,val dom:SchObs)

// case object SchSchema
// val schSchemaIsSchema: Schema[SchSchema.type] = new Schema[SchSchema.type] {
//   val name = "schSchemaIsSchema"

//   val emptySchema = SchSchema
//   type SchOb = SchObs
//   type SchHom = SchHoms


//   extension (s:SchSchema.type)
//     def obMap = SchObs.values.map(ob => ob.id -> ob).toMap
//     def homMap = SchHoms.values.map(f => f.id -> f).toMap
//     def attrMap = Map()

//     def renameElt(id0:UUID,name:String) = 
//       println("SchSchema is static")
//       s

//     def _addElts(elts:Seq[Generator]) = 
//       println("SchSchema is static")
//       s

//     def addProps(newProps:Seq[Property]) = 
//       println("SchSchema is static")
//       s

//     def _remElts(elts:Seq[Generator]) = 
//       println("SchSchema is static")
//       s

//     def remProps(oldProps:Seq[Property]) = 
//       println("SchSchema is static")
//       s



// }





// case class SchemaDisplay(
//   globalProps: PropMap,
//   props: Map[UUID,PropMap],
//   selected: Seq[UUID]
// ):
//   def setProps(id:UUID,newProps:PropMap) = this.copy(
//     props = props + (id -> newProps)
//   )


//   def setProp(id:UUID,f:Property,v:f.Value) = this.copy(
//     props = props + (id -> props(id).set(f,v))
//   )


//   def select(id:UUID) = this.copy(
//     selected = (selected :+ id).distinct
//   )
//   def unselect(ids:UUID*) = this.copy(
//     selected = selected.diff(ids)
//   )
//   def clear() = this.copy(
//     selected = Seq()
//   )

// object SchemaDisplay:
//   def apply(
//     gps: PropMap,
//     props:Map[UUID,PropMap],
//     selected:Seq[UUID]
//   ) = new SchemaDisplay(
//     gps,
//     props.withDefaultValue(PropMap()),
//     selected
//   )

//   def apply(): SchemaDisplay = SchemaDisplay(PropMap(),Map(),Seq())





// /* Schemas */



