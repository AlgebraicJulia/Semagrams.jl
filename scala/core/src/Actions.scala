package semagrams

import semagrams._
import semagrams.acsets.{given, _}
import semagrams.ui._
import semagrams.util._
import semagrams.widgets._
import monocle._

import com.raquo.laminar.api.L._
import cats.effect._
import semagrams.controllers.HoverController

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
    _ = es.hover.
    
    $state.set(HoverController.State(None))
  } yield ()

  // def drag(i: Part) = for {
  //   _ <- IO(m.save())
  //   _ <- IO(m.unrecord())
  //   _ <- m.updateS_(moveFront(i))
  //   c <- IO(m.now().subpart(Center, i))
  //   init <- es.mousePos
  //   offset <- IO(c - init)
  //   _ <- es.drag.drag(
  //     Observer(p => { m.update(_.setSubpart(i, Center, p + offset)) })
  //   )
  //   _ <- IO(m.record())
  // } yield ()



  def drag[Memo,Return](
      start: () => IO[Memo],
      during: (Memo,Complex) => Unit,
      end: Memo => IO[Return]
    ): Part => IO[Return] = (s:Part) => for {
      _ <- IO(m.save())
      _ <- IO(m.unrecord())
      m0 = m.now()
      memo <- start()
      ret <- (for {
        _ <- es.drag.drag(Observer(p => during(memo,p)))
        ret0 <- end(memo)
      } yield ret0).onCancelOrError(for
        _ <- IO(es.drag.$state.set(None))
        _ <- IO(m.set(m0))
      yield ())
      _ <- IO(m.record())
    } yield ret.asInstanceOf[Return]





  // Memo = Offset, Return = Unit
  def dragMove(i: Part) = 
    def start = () => for 
      _ <- m.updateS_(moveFront(i))
      c <- IO(m.now().subpart(Center, i))
      init <- es.mousePos
    yield c - init

    def during = (offset:Complex,p:Complex) => m.update(
      _.setSubpart(i, Center, p + offset)
    )

    def end = (offset:Complex) => IO(())

    drag(start,during,end)(i)



  val debug = IO(m.now()).flatMap(IO.println)
  def die[A]: IO[A] = fromMaybe(IO(None))
  

  // Memo = Return = wire
  // def makeWire(w:Ob,src:Hom,tgt:Hom) = (s:Part) => {

  //   def start = () => for 

  //   yield 
  // }


  // Memo = Part, Return = Part
  def dragEdge(
    ob: Ob,
    src: Hom,
    tgt: Hom,
  )(s: Part): IO[Part] = {

    def start = () => for
      p <- es.mousePos
      e <- if src.codoms.contains(s.ty)
        then m.updateS(addPart(ob, 
          PropMap().set(src,s).set(End, p).set(Interactable, false)
        ))
        else if tgt.codoms.contains(s.ty)
          then m.updateS(addPart(ob, 
            PropMap().set(tgt,s).set(Start, p).set(Interactable, false)
          ))
          else die
    yield e

    def during = (e:Part,p:Complex) => if m.now().hasSubpart(src,e)
      then m.update(_.setSubpart(e, End, p))
      else m.update(_.setSubpart(e, Start, p))

    def end = (e:Part) => for
      t <- fromMaybe(es.hoveredPart)
      out <- if m.now().hasSubpart(src,e) && tgt.codoms.contains(t.ty) 
        then m.updateS(for {
          _ <- setSubpart(e, tgt, t)
          _ <- remSubpart(e, End)
          _ <- remSubpart(e, Interactable)
        } yield e)
        else if m.now().hasSubpart(tgt,e) && src.codoms.contains(t.ty)
          then m.updateS(for {
            _ <- setSubpart(e, src, t)
            _ <- remSubpart(e, Start)
            _ <- remSubpart(e, Interactable)
          } yield e)
          else fromMaybe(IO(None))
    yield out

    drag(start,during,end)(s)

  }


  // def killDrag(m0:ACSet) = for
  // yield ()

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
