// package semagrams.state

// import com.raquo.laminar.api.L._
// import org.scalajs.dom


// /* A case class to handle key modifier state */
// case class ModState(
//     modifiers: Set[KeyModifier]
// ) {
//   def processEvent(evt: Event) = 
//     import Event._
//     evt match {
//       case KeyDown(key) => KeyModifier.fromString.get(key) match {
//         case Some(mod) => GlobalState(modifiers + mod)
//         case None => this
//       }
//       case KeyUp(key) => KeyModifier.fromString.get(key) match {
//         case Some(mod) => GlobalState(modifiers - mod)
//         case None => this
//       }
//       // Clear state when focus is lost
//       case Blur() => GlobalState(Set())
//       case _ => this
//   }
// }

// object GlobalState {
//   def listen(into: Observer[Event]) = {
//     dom.document.addEventListener("keydown",(ev:dom.KeyboardEvent) => into.onNext(KeyDown(ev.key)))
//     dom.document.addEventListener("keyup",(ev:dom.KeyboardEvent) => into.onNext(KeyUp(ev.key)))
//     dom.window.addEventListener("blur",(ev:dom.KeyboardEvent) => into.onNext(Blur()))
//   }
// }
