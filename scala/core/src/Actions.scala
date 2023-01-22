package semagrams

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.ui._
import semagrams.util._
import semagrams.widgets._

import com.raquo.laminar.api.L._
import cats.effect._

case class Actions(es: EditorState, m: Var[ACSet], ui: UIState) {
  import ACSet._

  def add(ob: Ob, props: PropMap) = for {
    pos <- es.mousePos
    _ <- m.updateS_(addPart(ob, props + (Center, pos)))
  } yield ()

  val del = for {
    ment <- es.hovered
    _ <- ment match {
      case Some(ent: Part) => m.updateS_(remPart(ent))
      case _               => IO(())
    }
  } yield ()

  // def drag(i: Part) = for {
  //   _ <- m.updateS_(ops.moveFront(i))
  //   c <- IO(m.now().subpart(Center, i))
  //   init <- es.mousePos
  //   offset <- IO(c - init)
  //   _ <- es.drag.dragStart(
  //     Observer(p => m.update(_.setSubpart(i, Center, p + offset)))
  //   )
  // } yield ()

  // def dragEdge[E1 <: Entity, E2 <: Entity](
  //   ob: Ob,
  //   src: PValue[E1],
  //   tgt: PValue[E2],
  //   tgtType: EntityType
  // )(s: E1) = for {
  //   p <- es.mousePos
  //   e <- m.updateS(
  //     addPart(
  //       ob,
  //       PropMap().set(src, s).set(End, p).set(Interactable, false)
  //     )
  //   )
  //   _ <- (for {
  //     _ <- es.drag.drag(Observer(p => m.update(_.setSubpart(e, End, p))))
  //     t <- fromMaybe(es.hoveredEntity(tgtType))
  //     _ <- m.updateS_(for {
  //       _ <- setSubpart(e, tgt, t.asInstanceOf[E2])
  //       _ <- remSubpart(End, e)
  //       _ <- remSubpart(Interactable, e)
  //     } yield ())
  //   } yield ()).onCancelOrError(for {
  //     _ <- IO(es.drag.$state.set(None))
  //     _ <- m.updateS_(remPart(e))
  //   } yield ())
  // } yield ()

  // def edit(p: Property { type Value = String; }, multiline: Boolean)(i: Part): IO[Unit] = for {
  //   _ <- IO(m.update(acs => if (acs.trySubpart(p, i).isEmpty) {
  //                      acs.setSubpart(p, i, "")
  //                    } else {
  //                      acs
  //                    }
  //           ))
  //   _ <- ui.addKillableHtmlEntity(
  //     kill => {
  //       val v = m.zoomL(ops.subpartLens(p, i))
  //       val t = TextInput(v, multiline)(kill)
  //       if (multiline) {
  //         PositionWrapper(Position.topToBotMid(10), t)
  //       } else {
  //         PositionWrapper(Position.botMid(10), t)
  //       }
  //     }
  //   )
  // } yield ()

  // def importExport = ui.addKillableHtmlEntity(
  //   kill => PositionWrapper(
  //     Position.topToBotMid(10),
  //     TextInput(m.zoomL(ops.serializedLens), true)(kill)
  //   )
  // )
}