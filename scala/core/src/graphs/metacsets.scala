package semagrams.graphs


import semagrams._
import semagrams.acsets._
import semagrams.util._


import upickle.default._




enum SchObs(val _name:String) extends Ob with Generator
  derives ReadWriter:
  case TableOb extends SchObs("TableOb")
  case ValTypeOb extends SchObs("ValTypeOb")
  case FKeyOb extends SchObs("FKeyOb")
  case AttrOb extends SchObs("AttrOb")
  val id = UUID(name)
  val generators = this.obGenerators

  name = _name
export SchObs._


enum SchHoms(name:String,val dom:SchObs,val codom:SchObs) 
  extends AbstractFKey with Generator:
  case FKeySrc extends SchHoms("FKeySrc",FKeyOb,TableOb)
  case FKeyTgt extends SchHoms("FKeyTgt",FKeyOb,TableOb)
  case AttrSrc extends SchHoms("AttrSrc",AttrOb,TableOb)
  case AttrTgt extends SchHoms("AttrTgt",AttrOb,ValTypeOb)

  val id = UUID(name)
  val generators = this.homGenerators
  val path = this.path
export SchHoms._


case object SchSchema extends Schema:

  val id = UUID("SchSchema")

  var name = "SchSchema"

  def elts: Map[UUID, Elt] = 
    SchObs.values.eltMap ++ SchHoms.values.eltMap


  def globalProps: Seq[Property] = Seq()





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





/* Schemas from ACSet(SchSchema) */



def schemaId(part:Part) = part.id.copy(
  stem = part.ob match
    case TableOb => "Table"
    case ValTypeOb => "ValType"
    case FKeyOb => "FKey"
    case AttrOb => "Attr"
    case _ => part.ob.toString
) 

  
def acsetSchema[D:PartData](acset:ACSet[D]): BasicSchema =
  if !acset.schema.hasElts(Seq[SchObs](TableOb,FKeyOb,AttrOb))
  then println(s"Warning: missing schema elements in $acset")
  val tableMap = acset.tryProp(Content,TableOb).map(
    (part,content) => content match
      case Some(name) => part.id -> Table(schemaId(part)).rename(name)
      case None => part.id -> Table(schemaId(part))
  )
    
  val fkeys = acset.getPropSeq(FKeyOb).collect { 
    case (part,props) if props.contains(FKeySrc,FKeyTgt) =>
      val s = props(FKeySrc)
      val t = props(FKeyTgt)

      FKey(
        schemaId(part),
        props.get(Content).getOrElse(""),
        tableMap(s.id),
        tableMap(t.id)
      )

  }

  val attrs = acset.getPropSeq(AttrOb).collect { 
    case (part,props) if props.contains(AttrSrc,AttrTgt) =>
      val s = props(AttrSrc)
      val t = props(AttrTgt)

      Attr(
        schemaId(part),
        props.get(Content).getOrElse(""),
        tableMap(s.id),
        ValType[String](t.id)
      )

  }


  BasicSchema((tableMap.values ++ fkeys ++ attrs).toSeq:_*)
