// package semagrams.acsets.nested

// import utest._
// import upickle.default._
// import semagrams.acsets._
// import semagrams._
// import semagrams.util._
// import cats.data.State

// import ACSet._
// import WiringDiagrams._

// object NestedACSetSpec extends TestSuite {

//   def tests = Tests {
//     // test("wiring diagram schema") {
//     //   assert(SchWiringDiagram.homsInto(PartType(Seq(Box, OutPort))).toSet ==
//     //            Set((Seq(), Src), (Seq(), OutSrc), (Seq(Box), OutTgt), (Seq(Box), ThroughTgt)))
//     // }
//     test("wiring diagrams") {
//       val mkWD = for {
//         b0 <- addPart(ROOT, Box)
//         p00 <- addPart(b0, OutPort)
//         b1 <- addPart(ROOT, Box)
//         p10 <- addPart(b1, InPort)
//         w0 <- addPart(ROOT, Wire)
//         _ <- setProp(w0, Src, p00)
//         _ <- setProp(w0, Tgt, p10)
//         _ <- State.modify[ACSet](
//           wd => {
//             assert(wd.hasPart(b0))
//             assert(wd.hasPart(p00))
//             assert(p00.path.length == 2)
//             assert(p10.path.length == 2)
//             assert(wd.subacset(b0).schema == SchWiringDiagram)
//             assert(wd.subacset(w0).schema == SchEmpty)
//             assert(wd.subpart(Src, w0) == p00)
//             assert(wd.subpart(Tgt, w0) == p10)
//             assert(wd.incident(p00, Src) == Seq(w0))
//             assert(wd.incident(p10, Tgt) == Seq(w0))
//             wd
//           }
//         )
//         b00 <- addPart(b0, Box)
//         p000 <- addPart(b00, OutPort)
//         w00 <- addPart(b0, Wire)
//         _ <- setProp(w00, Src, p000)
//         _ <- setProp(w00, Tgt, p00)
//         _ <- State.modify[ACSet](
//           wd => {
//             assert(p000.path.length == 3)
//             assert(wd.hasPart(p000))
//             assert(wd.subpart(Src, w00) == p000)
//             assert(wd.subpart(Tgt, w00) == p00)
//             assert(wd.incident(p00, Tgt) == Seq(w00))
//             assert(wd.incident(p00, Src) == Seq(w0))
//             wd
//           }
//         )
//         _ <- remPart(p00)
//         _ <- State.modify[ACSet](
//           wd => {
//             assert(!wd.hasPart(p00))
//             assert(!wd.hasPart(w0))
//             assert(!wd.hasPart(w00))
//             wd
//           }
//         )
//       } yield ()

//       mkWD.run(WiringDiagram()).value

//       ()
//     }
//   }
// }
