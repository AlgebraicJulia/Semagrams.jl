package semagrams

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.ui._
import semagrams.util._
import semagrams.widgets._
import monocle._

import com.raquo.laminar.api.L._
import cats.effect._

case class Actions(
  es: EditorState,
  m: UndoableVar[ACSet],
  ui: UIState,
  serialize: ACSet => String,
  deserialize: String => Option[ACSet],
) {
  import ACSet._

  def addAtMouse(ob: Ob, init: ACSet) = for {
    pos <- es.mousePos
    _ <- m.updateS_(addPart(ob, init.setSubpart(ROOT, Center, pos)))
  } yield ()

  def addAtMouse(ob: Ob) = for {
    pos <- es.mousePos
    _ <- m.updateS_(addPart(ob, PropMap() + (Center, pos)))
  } yield ()

  def add(part: Part, ob: Ob, props: PropMap) = for {
    i <- m.updateS(addPart(part, ob, props))
  } yield i

  def add_(part: Part, ob: Ob, props: PropMap) = for {
    _ <- m.updateS_(addPart(part, ob, props))
  } yield ()

  val del = for {
    ment <- es.hovered
    _ <- ment match {
      case Some(ent: Part) => m.updateS_(remPart(ent))
      case _               => IO(())
    }
  } yield ()

  def drag(i: Part) = for {
    _ <- IO(m.save())
    _ <- IO(m.unrecord())
    _ <- m.updateS_(moveFront(i))
    c <- IO(m.now().subpart(Center, i))
    init <- es.mousePos
    offset <- IO(c - init)
    _ <- es.drag.drag(
      Observer(p => { m.update(_.setSubpart(i, Center, p + offset)) })
    )
    _ <- IO(m.record())
  } yield ()

  val debug = IO(m.now()).flatMap(IO.println)

  def dragEdge(
    ob: Ob,
    src: Hom,
    tgt: Hom,
    promoteTgt: Option[Entity => IO[Part]] = None
  )(s: Part) = 
    for {
    _ <- IO(m.save())
    _ <- IO(m.unrecord())
    p <- es.mousePos
    e <- m.updateS(
      addPart(
        ob,
        PropMap().set(src, s).set(End, p).set(Interactable, false)
      )
    )
    _ <- (for {
      _ <- es.drag.drag(Observer(p => m.update(_.setSubpart(e, End, p))))
      t <- {
        println(s"dragEdge: $promoteTgt")
        promoteTgt match
          case None => fromMaybe(es.hoveredPart(tgt.codoms))
          case Some(f) => for {
            ent <- es.hovered
            _ = println(ent)
            p <- f(ent.getOrElse(Background()))
          } yield p
      }
      _ <- m.updateS_(for {
        _ <- setSubpart(e, tgt, t)
        _ <- remSubpart(e, End)
        _ <- remSubpart(e, Interactable)
      } yield ())
    } yield ()).onCancelOrError(for {
      _ <- IO(es.drag.$state.set(None))
      _ <- m.updateS_(remPart(e))
    } yield ())
    _ <- IO(m.record())
  } yield ()

  def edit(p: Property { type Value = String; }, multiline: Boolean)(i: Part): IO[Unit] = for {
    _ <- IO(m.update(acs => if (acs.trySubpart(p, i).isEmpty) {
                       acs.setSubpart(i, p, "")
                     } else {
                       acs
                     }
            ))
    _ <- ui.addKillableHtmlEntity(
      kill => {
        val v = m.zoomL(subpartLens(p, i))
        val t = TextInput(v, multiline)(kill)
        if (multiline) {
          PositionWrapper(Position.topToBotMid(10), t)
        } else {
          PositionWrapper(Position.botMid(10), t)
        }
      }
    )
  } yield ()

  def importExport = ui.addKillableHtmlEntity(
    kill => PositionWrapper(
      Position.topToBotMid(10),
      TextInput(m.zoomL(Lens(serialize)(s => a => deserialize(s).getOrElse(a))), true)(kill)
    )
  )

  
}
