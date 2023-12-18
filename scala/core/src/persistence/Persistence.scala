package semagrams.persistence

import io.laminext.fetch._
import upickle.default._
import concurrent.ExecutionContext.Implicits.global

import org.scalajs.dom

import semagrams._
import semagrams.util._
import semagrams.acsets._

case class Key(`val`: String) derives ReadWriter
case class RefResponse(key: Key, content: String) derives ReadWriter
case class ACSetVersion(ACSets: String, ACSetSchema: String) derives ReadWriter
case class CatlabOb(name: String) derives ReadWriter:
  def toTable() = Table(UID("CatlabOb"), name)

type Readable = String | Double | Int | Boolean
case class CatlabAttrType(name: String) derives ReadWriter:
  def toValType(): ValType[_ <: Readable] =
    val id = UID("CatlabType")
    name match
      case "String" | "Label" | "Name"    => ValType[String](id, name)
      case "Float" | "Float32" | "Double" => ValType[Double](id, name)
      case "Int" | "Nat" | "Int32"        => ValType[Int](id, name)
      case "Bool" | "Boolean"             => ValType[Boolean](id, name)
      case _                              => ValType[String](id, name)

case class CatlabHom(name: String, dom: CatlabOb, codom: CatlabOb):
  def toFKey(dom: Table, codom: Table) =
    FKey(UID("CatlabHom"), name, dom, codom)
object CatlabHom:
  implicit val rw: ReadWriter[CatlabHom] =
    readwriter[Map[String, String]].bimap(
      f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
      m => CatlabHom(m("name"), CatlabOb(m("dom")), CatlabOb(m("codom")))
    )
case class CatlabAttr(name: String, dom: CatlabOb, codom: CatlabAttrType):
  def toAttr(dom: Table, codom: ValType[_]) =
    Attr[codom.PartType](UID("CatlabAttr"), name, dom, codom)(codom.rw)

object CatlabAttr:
  implicit val rw: ReadWriter[CatlabAttr] =
    readwriter[Map[String, String]].bimap(
      f => Map("name" -> f.name, "dom" -> f.dom.name, "codom" -> f.codom.name),
      m => CatlabAttr(m("name"), CatlabOb(m("dom")), CatlabAttrType(m("codom")))
    )

case class CatlabSchema(
    version: ACSetVersion,
    Ob: Seq[CatlabOb],
    AttrType: Seq[CatlabAttrType],
    Hom: Seq[CatlabHom],
    Attr: Seq[CatlabAttr]
) derives ReadWriter:

  def toSchema(id: UID): BasicSchema =
    val obs = Ob.map(x => x -> x.toTable()).toMap
    val attrtypes: Map[CatlabAttrType, ValType[_]] =
      AttrType.map(t => t -> t.toValType()).toMap
    val homs = Hom.map(f => f.toFKey(obs(f.dom), obs(f.codom)))
    val attrs: Seq[Attr[_]] =
      Attr.map(a =>
        a.toAttr(
          obs(a.dom),
          attrtypes(a.codom)
        )
      )

    val elts = (obs.values ++ attrtypes.values ++ homs ++ attrs).eltMap
    BasicSchema(
      id,
      elts
    )

import ujson.Value.JsonableDict

def readSchema(resp: RefResponse): BasicSchema =
  val content = dom.window.atob(resp.content)
  val cl = read[CatlabSchema](content)
  val sch = cl.toSchema(UID("CatlabSchema"))
  println(s"Read schema $sch")
  sch

val fetch = Fetch
  .post(
    "http://localhost:5179",
    write(Map("_type" -> "ReadRef", "ref" -> "schema"))
  )
  .text
