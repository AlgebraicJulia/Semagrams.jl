package semagrams.examples

// import com.raquo.laminar.api.L._
// import semagrams.sprites._
// import semagrams._
// import semagrams.util._
// import semagrams.controllers._
// import cats.data._
// import cats.syntax.all._

// case class Boxes(
//   boxes: Map[Int, BoxData],
//   counter: Int,
//   defaultDims: Complex
// ) {
//   def processBoxEvent(evt: BoxEvent) = {
//     evt match {
//       case BoxEvent.NewParams(ent, params) => this.copy(boxes = boxes + (ent.id -> params))
//     }
//   }

//   def addBox(pos: Complex) = this.copy(boxes = boxes + (counter -> BoxData(defaultDims, pos - defaultDims / 2)), counter = counter + 1)

//   def remBox(id: Int) = this.copy(boxes = boxes - id)

//   def spriteData = this.boxes.map((id, bd) => (Entity("box", id), bd)).toList
// }

// object Boxes {
//   def apply() = {
//     new Boxes(Map(), 0, Complex(50, 50))
//   }
// }

// val addBoxAction: Action[Boxes, Unit] = for {
//   pos <- mousePos
//   _ <- updateModel[Boxes](_.addBox(pos))
// } yield {}

// val remBoxAction: Action[Boxes, Unit] = (for {
//   ent <- OptionT(hovered)
//   _ <- OptionT.liftF(updateModel[Boxes](_.remBox(ent.id)))
// } yield {}).value.map(_ => {})

// val bindings = KeyBindings(
//   Map(
//     "a" -> addBoxAction,
//     "d" -> remBoxAction
//   )
// )


// case class StaticBoxes($state: Var[Boxes]) {
//   def present() = {
//     val elt = baseSvg
//     val es = EditorState($state, elt)

//     val boxEvents = EventBus[BoxEvent]()
//     val boxSprite = Box(es.hover, es.drag, boxEvents.writer)
//     val boxSprites = SpriteCollection(boxSprite, $state.signal.map(_.spriteData))

//     elt.amend(
//       boxEvents --> $state.updater[BoxEvent]((state, evt) => state.processBoxEvent(evt)),
//       boxSprites
//     )

//     runAction(es, bindings.run.foreverM)
//     elt
//   }
// }
