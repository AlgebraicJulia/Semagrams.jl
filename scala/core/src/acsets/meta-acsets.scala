package semagrams.acsets

import semagrams._
import semagrams.acsets._
import semagrams.util._
import semagrams.partprops._

import upickle.default._

enum SchObs(val _name: String) extends Ob with Generator derives ReadWriter:
  case TableOb extends SchObs("TableOb")
  case ValTypeOb extends SchObs("ValTypeOb")
  case FKeyOb extends SchObs("FKeyOb")
  case AttrOb extends SchObs("AttrOb")

  type PartType = Part
  val id = UID(name)
  val generators = this.obGenerators

  name = _name
export SchObs._

enum SchHoms(name: String, val dom: SchObs, val codom: SchObs)
    extends PartHom
    with Generator:
  case FKeySrc extends SchHoms("FKeySrc", FKeyOb, TableOb)
  case FKeyTgt extends SchHoms("FKeyTgt", FKeyOb, TableOb)
  case AttrSrc extends SchHoms("AttrSrc", AttrOb, TableOb)
  case AttrTgt extends SchHoms("AttrTgt", AttrOb, ValTypeOb)

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

def acset2Schema(id: UID)(acset: ACSet): BasicSchema =
  if !acset.schema.hasElts(Seq[SchObs](TableOb, FKeyOb, AttrOb))
  then println(s"Warning: missing schema elements in $acset")
  val tableMap = acset
    .tryProp(Content, TableOb)
    .map((part, content) =>
      content match
        case Some(name) => part.id -> Table(part.id).rename(name)
        case None       => part.id -> Table(part.id)
    )

  val fkeys = acset.getPropSeq(FKeyOb).collect {
    case (part, props) if props.contains(FKeySrc, FKeyTgt) =>
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
    case (part, props) if props.contains(AttrSrc, AttrTgt) =>
      val s = props(AttrSrc)
      val t = props(AttrTgt)

      Attr(
        part.id,
        props.get(Content).getOrElse(""),
        tableMap(s.id),
        ValType[String](t.id)
      )

  }

  BasicSchema(id, (tableMap.values ++ fkeys ++ attrs).toSeq: _*)

def schema2ACSet(schema: BasicSchema): ACSet =
  val a0 = ACSet(SchSchema)

  val (a1, obparts) = a0.addPartsById(
    TableOb,
    schema.tables.map((id, ob) => id -> PropMap().set(Content, ob.name))
  )
  val obs = obparts.map(p => p.id -> p).toMap

  val (a2, tpparts) = a1.addPartsById(
    ValTypeOb,
    schema.attrSeq
      .map(_.codom)
      .eltMap
      .map((id, tp) => id -> PropMap().set(Content, tp.name))
  )
  val tps = tpparts.map(tp => tp.id -> tp).toMap

  val fkeys = schema.fkeys.map((id, f) =>
    id -> PropMap()
      .set(Content, f.name)
      .set(FKeySrc, obs(f.dom.id))
      .set(FKeyTgt, obs(f.codom.id))
  )
  val a3 = a2.addPartsById(FKeyOb, fkeys)._1

  val attrs = schema.attrs.map((id, a) =>
    id -> PropMap()
      .set(Content, a.name)
      .set(AttrSrc, obs(a.dom.id))
      .set(AttrTgt, tps(a.codom.id))
  )
  val a4 = a3.addPartsById(AttrOb, attrs)._1

  a4