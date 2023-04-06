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
import semagrams.acsets.WiringDiagrams.OutPort
import semagrams.acsets.WiringDiagrams.InPort
import semagrams.sprites.Middleware
import semagrams.sprites.WithMiddleware
import semagrams.sprites.DPBox
import semagrams.sprites.Rect

/** This class bundles the common arguments to many actions one might want to do
  * in a binding so that you don't have to pass them in every time, and then
  * provides a bunch of commonly used actions as methods.
  */
case class Actions(
    es: EditorState,
    m: UndoableVar[ACSet],
    ui: UIState,
    serialize: ACSet => String,
    deserialize: String => Option[ACSet]
) {
  import ACSet._

  /** Add and return a part to the model with type `Ob` and subacset given by
    * `init`, and set the [[Center]] of that new part to be the current mouse
    * position
    */
  def addAtMouse(ob: Ob, init: ACSet): IO[Part] = for {
    pos <- es.mousePos
    x <- m.updateS(addPart(ob, init.setSubpart(ROOT, Center, pos)))
  } yield x

  /** Add and return a part to the model with type `Ob` and [[Center]] the
    * current mouse position.
    */
  def addAtMouse(ob: Ob): IO[Part] = for {
    pos <- es.mousePos
    x <- m.updateS(addPart(ob, PropMap() + (Center, pos)))
  } yield x

  /** [[addAtMouse]] without returning the part */
  def addAtMouse_(ob: Ob, init: ACSet): IO[Unit] =
    addAtMouse(ob, init).map(_ => ())

  /** [[addAtMouse]] without returning the part */
  def addAtMouse_(ob: Ob): IO[Unit] = addAtMouse(ob).map(_ => ())

  /** Add and return a part to the acset at `p` of type `ob` with properties
    * `props`
    */
  def add(p: Part, ob: Ob, props: PropMap): IO[Part] = m.updateS(
    addPart(p, ob, props)
  )

  /** [[add]] without returning the part */
  def add_(p: Part, ob: Ob, props: PropMap): IO[Unit] =
    add(p, ob, props).map(_ => ())

  /** Set the value of the property `f` at the part `p` to be `v` */
  def set(p: Part, f: Property, v: f.Value): IO[Unit] = m.updateS(
    setSubpart(p, f, v)
  )

  /** Unset the value of the property `f` of the part `p` */
  def remove(p: Part, f: Property): IO[Unit] = m.updateS(
    remSubpart(p, f)
  )

  /** Remove the part `p` */
  def remove(p: Part): IO[Unit] = m.updateS(
    remPart(p)
  )

  /** Remove the part currently hovered */
  val del = for {
    ment <- es.hovered
    _ <- ment match {
      case Some(p: Part) => remove(p)
      case _             => IO(())
    }
    _ = es.hover.$state.set(HoverController.State(None))
  } yield ()

  /** Get the bounding box of `b` as it is currently displayed on the screen */
  def getBBox(b: Part): IO[BoundingBox] = b.path match
    case Seq() =>
      val sz = es.size.now()
      IO(BoundingBox(sz / 2.0, sz))
    case Seq(head) =>
      for {
        sprtry <- IO(es.entities.now().em.get(b))
        sprdata <- sprtry match
          case Some(pair) => fromMaybe(IO(sprtry))
          case None =>
            die
        (spr, acset) = sprdata
        bbtry = spr.bbox(Part(Nil), acset)
        bb <- {
          bbtry match
            case Some(bb) => IO(bb)
            case None =>
              die
        }
      } yield bb
    case Seq(head, rest @ _*) =>
      for {
        bdata <- fromMaybe(IO(es.entities.now().em.get(b.head)))
        (spr, acset) = bdata
        bb <- getBBox(ROOT)
      } yield bb

  /** A generalized drag action
    *
    * At the beginning, compute a value `memo` of type `Memo` from the mouse
    * position.
    *
    * While the drag is happening, execute `during` with `memo` and the mouse
    * position of the drag.
    *
    * Finally, call `after` with `memo` to get the return value.
    *
    * If the drag is canceled, this resets the state.
    *
    * @todo
    *   - It seems like we do not use the parameter `s` anywhere; we may want to
    *     remove it
    *   - `during` executes for side-effects, which could be anything! But we
    *     are assuming that all it does is change `m`. We should express this
    *     explicitly by having `during` return a function `ACSet => ACset`, and
    *     then hook that into modifying `m`.
    *   - Is `drag` the best name for this?
    */
  def drag[Memo, Return](
      start: (z: Complex) => IO[Memo],
      during: (Memo, Complex) => Unit,
      after: Memo => IO[Return]
  )(s: Part): IO[Return] = for {
    _ <- IO(m.save())
    _ <- IO(m.unrecord())
    m0 = m.now()
    memo <- es.mousePos.flatMap(start)
    ret <- (es.drag.drag(Observer(p => during(memo, p))) >> after(memo))
      .onCancelOrError(IO({ es.drag.$state.set(None); m.set(m0) }))
    _ <- IO(m.record())
  } yield ret.asInstanceOf[Return]

  /** Drag to move a part around */
  def dragMove(i: Part) =
    def start = (z: Complex) =>
      for
        _ <- m.updateS_(moveFront(i))
        c <- IO(m.now().subpart(Center, i))
      yield c - z

    def during = (offset: Complex, p: Complex) =>
      m.update(
        _.setSubpart(i, Center, p + offset)
      )

    def after = (offset: Complex) => IO(())

    drag(start, during, after)(i)

  val debug = IO(m.now()).flatMap(IO.println)
  def die[A]: IO[A] = fromMaybe(IO(None))

  def liftTo(p: Part, tp: PartType): IO[Part] =
    tp.path match
      case Seq() =>
        IO(p)
      case Seq(next, rest @ _*) => {
        for
          ext <- add(p, next, PropMap())
          last <- liftTo(ext, PartType(rest))
        yield last
      }

  def noLift(p: Part, pos: Complex): PartType = ROOT.ty

  /** Drag to construct an edge between two parts
    *
    * Returns the part corresponding to the edge that was constructed
    */
  def dragEdge(
      ob: Ob,
      src: Hom,
      tgt: Hom,
      liftSeq: (Part, Complex) => PartType = noLift
  )(s: Part): IO[Part] =

    def start = (z: Complex) =>
      val ptype = liftSeq(s, z)
      for
        p <- liftTo(s, ptype)
        w <- add(
          ROOT,
          ob,
          p.ty match
            case tp if src.codoms.contains(tp) =>
              PropMap()
                .set(src, p)
                .set(End, z)
                .set(Interactable, false)
            case tp if tgt.codoms.contains(tp) =>
              PropMap()
                .set(tgt, p)
                .set(Start, z)
                .set(Interactable, false)
        )
      yield w

    def during = (e: Part, p: Complex) =>
      if m.now().hasSubpart(src, e)
      then m.update(_.setSubpart(e, End, p))
      else m.update(_.setSubpart(e, Start, p))

    def after = (e: Part) =>
      for
        t <- fromMaybe(es.hoveredPart)
        z <- es.mousePos
        qtype = liftSeq(t, z)
        q <- liftTo(t, qtype)
        _ <- m.now() match
          case mnow if mnow.hasSubpart(src, e) =>
            for {
              _ <- set(e, tgt, q)
              _ <- remove(e, End)
              _ <- remove(e, Interactable)
            } yield ()
          case mnow if mnow.hasSubpart(tgt, e) =>
            for {
              _ <- set(e, src, q)
              _ <- remove(e, Start)
              _ <- remove(e, Interactable)
            } yield ()
          case _ => die
      yield e

    drag(start, during, after)(s)

  /** Edit the content of the part `i`, using popup text box */
  def edit(p: Property { type Value = String; }, multiline: Boolean)(
      i: Part
  ): IO[Unit] = for {
    _ <- IO(
      m.update(acs =>
        if (acs.trySubpart(p, i).isEmpty) {
          acs.setSubpart(i, p, "")
        } else {
          acs
        }
      )
    )
    _ <- ui.addKillableHtmlEntity(kill => {
      val v = m.zoomL(subpartLens(p, i))
      val t = TextInput(v, multiline)(kill)
      if (multiline) {
        PositionWrapper(Position.topToBotMid(10), t)
      } else {
        PositionWrapper(Position.botMid(10), t)
      }
    })
  } yield ()

  /** Bring up a textbox that can be used for copy/pasting the serialized
    * version of the current state
    */
  def importExport = ui.addKillableHtmlEntity(kill =>
    PositionWrapper(
      Position.topToBotMid(10),
      TextInput(
        m.zoomL(Lens(serialize)(s => a => deserialize(s).getOrElse(a))),
        true
      )(kill)
    )
  )

}
