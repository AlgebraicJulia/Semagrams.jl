package semagrams

import semagrams._
import semagrams.acsets.{given, _}

import com.raquo.laminar.api.L._
import cats.effect._

case class Actions[S: IsSchema](es: EditorState, m: Var[ACSet[S]]) {
  val ops = summon[ACSetOps[S]]

  def add(ob: Ob) = for {
    pos <- es.mousePos
    _ <- m.updateS_(ops.addPart(ob, PropMap() + (Center, pos)))
  } yield ()

  val del = for {
    ment <- es.hovered
    _ <- ment match {
      case Some(ent: Part) => m.updateS_(ops.remPart(ent))
      case _ => IO(())
    }
  } yield ()

  def drag(i: Part) = for {
    c <- IO(m.now().subpart(Center, i))
    init <- es.mousePos
    offset <- IO(c - init)
    _ <- es.drag.dragStart(
      Observer(p => m.update(_.setSubpart(Center, i, p + offset)))
    )
  } yield ()

  def dragEdge(ob: Ob, src: Hom, tgt: Hom)(s: Part) = for {
    p <- es.mousePos
    e <- m.updateS(ops.addPart(ob, PropMap().set(src, s).set(End, p).set(Interactable, false)))
    _ <- (for {
      _ <- es.drag.drag(Observer(p => m.update(_.setSubpart(End, e, p))))
      t <- fromMaybe(es.hoveredPart(tgt.codom))
      _ <- m.updateS_(for {
                        _ <- ops.setSubpart(tgt, e, t)
                        _ <- ops.remSubpart(End, e)
                        _ <- ops.remSubpart(Interactable, e)
                      } yield ())
    } yield ()).onCancelOrError(for {
      _ <- IO(es.drag.$state.set(None))
      _ <- m.updateS_(ops.remPart(e))
    } yield ())
  } yield ()
}
