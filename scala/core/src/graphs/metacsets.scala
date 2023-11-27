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
  val id = UID(name)
  val generators = this.obGenerators

  name = _name
export SchObs._


enum SchHoms(name:String,val dom:SchObs,val codom:SchObs) 
  extends AbstractFKey with Generator:
  case FKeySrc extends SchHoms("FKeySrc",FKeyOb,TableOb)
  case FKeyTgt extends SchHoms("FKeyTgt",FKeyOb,TableOb)
  case AttrSrc extends SchHoms("AttrSrc",AttrOb,TableOb)
  case AttrTgt extends SchHoms("AttrTgt",AttrOb,ValTypeOb)

  val id = UID(name)
  val generators = this.homGenerators
  val path = this.path
export SchHoms._


case object SchSchema extends Schema:

  val id = UID("SchSchema")

  var name = "SchSchema"

  def elts: Map[UID, Elt] = 
    SchObs.values.eltMap ++ SchHoms.values.eltMap


  def globalProps: Seq[Property] = Seq()








/* Schemas from ACSet(SchSchema) */


  
def acsetSchema[D:PartData](id:UID)(acset:ACSet[D]): BasicSchema =
  if !acset.schema.hasElts(Seq[SchObs](TableOb,FKeyOb,AttrOb))
  then println(s"Warning: missing schema elements in $acset")
  val tableMap = acset.tryProp(Content,TableOb).map(
    (part,content) => content match
      case Some(name) => part.id -> Table(part.id).rename(name)
      case None => part.id -> Table(part.id)
  )
    
  val fkeys = acset.getPropSeq(FKeyOb).collect { 
    case (part,props) if props.contains(FKeySrc,FKeyTgt) =>
      val s = props(FKeySrc)
      val t = props(FKeyTgt)

      FKey(
        part.id,
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
        part.id,
        props.get(Content).getOrElse(""),
        tableMap(s.id),
        ValType[String](t.id)
      )

  }


  BasicSchema(id,(tableMap.values ++ fkeys ++ attrs).toSeq:_*)
