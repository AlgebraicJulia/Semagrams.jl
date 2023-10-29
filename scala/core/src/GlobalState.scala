// package semagrams

// import com.raquo.laminar.api.L._

// case class GlobalState(
//     modifiers: Set[KeyModifier]
// ) {
//   def processEvent(evt: Event) = 
//     evt match {
//     case Event.KeyDown(key) => KeyModifier.fromString.get(key) match {
//       case Some(mod) => GlobalState(modifiers + mod)
//       case None => this
//     }
//     case Event.KeyUp(key) => KeyModifier.fromString.get(key) match {
//       case Some(mod) => GlobalState(modifiers - mod)
//       case None => this
//     }
//     // Clear state when focus is lost
//     case Blur() => GlobalState(Set())
//     cae Click(ent,button,_)
//     case _ => this
//   }
// }

// object GlobalState {
//   def listen(into: Observer[Event]) = {
//     dom.document.addEventListener("keydown",(ev:KeyboardEvent) => into.onNext(KeyDown(ev.key)))
//     dom.document.addEventListener("keyup",(ev:KeyboardEvent) => into.onNext(KeyUp(ev.key)))
//     dom.window.addEventListener("blur",(ev:KeyboardEvent) => into.onNext(Blur()))
//   }
// }
