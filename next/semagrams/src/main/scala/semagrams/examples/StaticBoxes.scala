package semagrams.examples

import com.raquo.laminar.api.L._
import semagrams.SemagramsEditor
import semagrams.sprites._
import semagrams._
import semagrams.Sprite.SpriteCollection
import semagrams.controllers.HoverController
import semagrams.controllers.DragController

case class StaticBoxesInit(
  hoverHandle: ControllerHandle[Option[Entity]]
)

class StaticBoxes extends SemagramsEditor {
  type Model = Map[Entity, BoxData]
  type InitData = StaticBoxesInit

  def updateModel(state: EditorState[Model,InitData], e: EditorEvent) = {
    val newModel = e match {
      case MoveEntityEvent(ent, pos) => {
        val bd = state.model(ent)
        state.model + (ent -> bd.copy(pos = pos))
      }
      case _ => state.model
    }
    Outcome.success(newModel)
  }

  def spriteCollections(initData: InitData, $model: Signal[Model]) = {
    List(
      SpriteCollection(
        Box(initData.hoverHandle),
        $model.map(_.toList)
      )
    )
  }

  def initialize(model: Model) = {
    val regBuilder = RegistryBuilder()
    val hoverHandle = regBuilder.addController(HoverController())
    regBuilder.addController(DragController())
    (StaticBoxesInit(hoverHandle), regBuilder.build())
  }
}
