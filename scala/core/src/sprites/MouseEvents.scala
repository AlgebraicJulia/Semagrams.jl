package semagrams.sprites

import semagrams._
import com.raquo.laminar.api.L._
import semagrams.acsets.abstr.Part

object MouseEvents {
  def hoverHandlers(
      ent: Part,
      eventWriter: Observer[Event]
  ): Seq[Mod[SvgElement]] =
    Seq(
      onMouseEnter.mapTo(MouseEnter(ent)) --> eventWriter,
      onMouseLeave.mapTo(MouseLeave(ent)) --> eventWriter
    )

  def clickHandlers(
      ent: Part,
      eventWriter: Observer[Event]
  ): Seq[Mod[SvgElement]] =
    Seq(
      onContextMenu.stopPropagation.preventDefault.map(evt =>
        ContextMenu(Some(ent))
      ) --> eventWriter,
      onMouseDown.stopPropagation.map(evt =>
        MouseDown(Some(ent), MouseButton.fromJS(evt.button))
      ) --> eventWriter,
      onMouseUp.stopPropagation.map(evt =>
        MouseUp(Some(ent), MouseButton.fromJS(evt.button))
      ) --> eventWriter,
      onDblClick.preventDefault.stopPropagation.map(evt =>
        DoubleClick(Some(ent), MouseButton.fromJS(evt.button))
      ) --> eventWriter
    )

  def handlers(
      ent: Part,
      eventWriter: Observer[Event]
  ): Seq[Mod[SvgElement]] =
    Seq(
      hoverHandlers(ent, eventWriter),
      clickHandlers(ent, eventWriter)
    )
}
