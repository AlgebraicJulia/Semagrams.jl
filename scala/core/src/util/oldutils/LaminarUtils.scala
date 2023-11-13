// package semagrams.util

// import com.raquo.laminar.api.L._
// import com.raquo.laminar.nodes.ReactiveElement
// import semagrams._

// extension [A, B](s: Signal[Tuple2[A, B]])
//   def splitTuple: Tuple2[Signal[A], Signal[B]] = (s.map(_._1), s.map(_._2))


// def printObs(a:Any) = Observer(_ => println(a.toString))



// // case class LaminarInterface[S,T](
// //   state: Signal[S],
// //   update: S => T => S
// // ):
// //   def lens[A,B](
// //     readout: S => A,
// //     update: S => B => T
// //   ): LaminarInterface[A,B] = LaminarInterface(
// //     state.map(readout),
// //     s => msgObs(s).contramap(update(s))
// //   )


// // trait Component[State,Msg]:
// //   val state: Signal[State]
// //   val update: Observer[Msg]

// //   def render(): Element
  
// //   // val interface = LaminarInterface(state,update)

// // case class TableComponent[K](
// //   state:Signal[Seq[(K,PropMap)]],
// //   update:Observer[(K,PropVal[_])] => Unit
// // ) extends Component[Seq[(K,PropMap)],(K,PropVal[_])]:
  
// //   // val rowSig = state.split(_._1){ case (k,(_,props),pairSig) =>
// //   //   TableRow
// //   // }

// //   def render() = table(

// //   )
  
  
//   // map(rows => table(
//   //   rows.map((k,props) => TableRow(k,props).render)
//   // )




// // case class TableComponent(iface:LaminarInterface[Seq[PropMap],(Int,PropVal[_])]) extends Component(iface):

// //   def render(pmaps:Seq[PropMap]) = table(
// //     pmaps.map()
// //   )

// trait Component:
//   def render(): Element

// case class TableComp[K](rows:Signal[Seq[(K,PropMap)]]) extends Component:
//   def render() = table(
//     children <-- rows.split(_._1)( (k,pair,pairSig) =>
//       TableRow(k,pairSig.map(_._2)).render()
//     )
    
//   )
// case class TableRow[K](k:K,props:Signal[PropMap]) extends Component:
//   def render() = tr(
//     td(k.toString),
//     children <-- props.map(p => p.pmap.toSeq)
//       .split(_._1)( (k,pair,pairSig) =>
//         TableCell(pair._2.toString).render()
//       )
//   )
// case class TableCell(state:String) extends Component:
//   def render() = td(state)