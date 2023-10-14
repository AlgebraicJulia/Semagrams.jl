package acsets

import acsets.Scopes._

val emptyScope = Scope(handle => {})

// val graphSchema = Scope(
//   handle => {
//     val ob = handle.bind("Ob", Type.Sort(emptyScope.tag))
//     val homOps = Scope(
//       handle => {
//         handle.bind("src", Type.Entity(ob))
//         handle.bind("tgt", Type.Entity(ob))
//       }
//     )
//     handle.bind("Hom", Type.Sort(homOps.tag))
//   }
// )

// val Ob = graphSchema("Ob")
// val Hom = graphSchema("Hom")
// val src = Hom.operations("src")
// val tgt = Hom.operations("tgt")

// val g = Scope(
//   handle => {
//     handle.parent(graphSchema)
//     val x = handle.bind(Ob)
//     val y = handle.bind(Ob)
//     val e = handle.bind(Hom)
//     handle.set(e, src, x)
//     handle.set(e, tgt, y)
//   }
// )

// val ddsSchema = Scope(
//   _.bind(
//     "X", X => {
//       val ops = Scope(_.bind("next", Type.Entity(X)))
//       Type.Sort(ops.tag)
//     }
//   )
// )
