// package semagrams.acsets

// import upickle.default._

// /** A catlab-compatible implementation of `Schema`.
//   *
//   * Used for apps like category of elements display, where we have to accept an
//   * arbitrary acset from Catlab.
//   */
// object catlab {
//   case class Version(
//       @upickle.implicits.key("ACSetSchema")
//       acsetSchema: String,
//       @upickle.implicits.key("Catlab")
//       catlab: String
//   )

//   given ReadWriter[Version] = macroRW

//   case class Ob(name: String)
//   given ReadWriter[Ob] = macroRW
//   case class Hom(name: String, dom: String, codom: String)
//   given ReadWriter[Hom] = macroRW
//   case class AttrType(name: String)
//   given ReadWriter[AttrType] = macroRW
//   case class Attr(name: String, dom: String, codom: String)
//   given ReadWriter[Attr] = macroRW

//   case class Schema(
//       version: Version,
//       @upickle.implicits.key("Ob")
//       obs: Seq[Ob],
//       @upickle.implicits.key("Hom")
//       homs: Seq[Hom],
//       @upickle.implicits.key("AttrType")
//       attrtypes: Seq[AttrType],
//       @upickle.implicits.key("Attr")
//       attrs: Seq[Attr]
//   )
//   given ReadWriter[Schema] = macroRW

//   type ACSet = Map[String, Seq[Map[String, ujson.Value]]]
// }
