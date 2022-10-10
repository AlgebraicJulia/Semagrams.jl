package semagrams.actions

import com.raquo.laminar.api.L._
import semagrams.util._
import semagrams.acsets.{*, given}
import semagrams.actions._
import semagrams.controllers._
import semagrams.text._
import semagrams.sprites._
import cats.data._
import cats.effect._
import cats.syntax.all._
import cats.effect.syntax.all._

import Action.ops

def addEntityPos[A: ACSet, X <: Ob](
    x: X,
    init: Elt[X] => State[WithProps[A], Unit]
): Action[WithProps[A], Elt[X]] = for {
  pos <- mousePos
  v <- updateModelS[WithProps[A], Elt[X]](
    for {
      v <- addPartWP(x, PropMap() + (Center, pos))
      _ <- init(v)
    } yield v
  )
  _ <- update
} yield v

def remEntity[A: ACSet]: Action[A, Unit] = for {
  v <- fromMaybe(hovered)
  _ <- updateModel[A](_.remPart(v))
  _ <- update
} yield ()

def dragEdge[A: ACSet, X <: Ob, Y <: Ob, Z <: Ob](
    src: Hom[X, Y],
    tgt: Hom[X, Z],
    s: Elt[Y]
)(implicit withPropsACSet: ACSet[WithProps[A]]): Action[WithProps[A], Unit] = {
  for {
    drag <- ops[WithProps[A]].ask.map(_.drag)
    $model <- ops[WithProps[A]].ask.map(_.$model)
    p <- mousePos
    e <- updateModelS[WithProps[A], Elt[X]](for {
      e <- addPartWP(src.dom, PropMap() + (End, p))
      _ <- setSubpart(src, e, s)
    } yield e)
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setProp(e, End, p))))
      t <- fromMaybe(hoveredPart(tgt.codom.asInstanceOf[Z]))
      _ <- updateModelS[WithProps[A], Unit](setSubpart(tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- ops.delay(drag.$state.set(None))
      _ <- updateModelS[WithProps[A], Unit](remPart(e))
    } yield ())
    _ <- update
  } yield ()
}

def dragControl[A: ACSet, X <: Ob](attr: Attr[X, Double], increment: Double)(
    v: Elt[X]
): Action[A, Unit] = {
  for {
    drag <- ops[A].ask.map(_.drag)
    $model <- ops[A].ask.map(_.$model)
    info <- ops[A].ask.map(_.bottomtip)
    p <- mousePos
    init <- ops.delay($model.now().subpart(attr, v).get)
    _ <- ops.delay(info.show(attr.toString() + ": " + "%.2f".format(init)))
    _ <- drag.drag(
        Observer(q => {
          val newval = (init + (p.y - q.y) * increment).max(0)
          $model.update(
            _.setSubpart(attr, v, newval)
          )
          info.show(attr.toString() + ": " + "%.2f".format(newval))
        })
      )
      .onCancelOrError(for {
        _ <- ops[A].delay(drag.$state.set(None))
        _ <- hideInfo
      } yield ())
    _ <- hideInfo
    _ <- update
  } yield ()
}

def loopDuringPress[A: ACSet](
    key: String,
    action: Action[WithProps[A], Unit]
): Action[WithProps[A], Unit] = for {
  target <- ops[WithProps[A]].foreverM(action).start
  _ <- Bindings(keyUp(key)).run
  _ <- target.cancel
} yield {}

def loopUntilPress[A: ACSet](
    key: String,
    action: Action[WithProps[A], Unit]
): Action[WithProps[A], Unit] = for {
  target <- ops[WithProps[A]].foreverM(action).start
  _ <- Bindings(keyDown(key)).run
  _ <- target.cancel
} yield {}

def dragEdgeLoop[A: ACSet, X <: Ob, Y <: Ob, Z <: Ob](
    src: Hom[X, Y],
    tgt: Hom[X, Z],
    key: String
)(implicit withPropsACSet: ACSet[WithProps[A]]): Action[WithProps[A], Unit] = {
  val mainAction: Action[WithProps[A], Unit] = for {
    s <- fromMaybe(
      Bindings(
        clickOn(ClickType.Single, MouseButton.Left, src.codom.asInstanceOf[Y])
      ).run
    )
    _ <- dragEdge(src, tgt, s)
  } yield ()

  loopDuringPress(key, mainAction)
}

def editContent[A: ACSet, X <: Ob](
    x: X
)(v: Elt[X]): Action[WithProps[A], Unit] =
  for {
    $model <- getModel[WithProps[A]]
    _ <- editText(
      Observer(s => $model.update(m => { m.setProp(v, Content, s) })),
      $model.now().getProp(v, Content)
    )
  } yield {}

def editStringAttr[A: ACSet, X <: Ob](
    x: X,
    attr: Attr[X, String]
)(v: Elt[X]): Action[A, Unit] =
  for {
    $model <- getModel[A]
    _ <- editText(
      Observer(s => $model.update(m => { m.setSubpart(attr, v, s) })),
      $model.now().subpart(attr, v).getOrElse("")
    )
  } yield {}

def editStringAttrBlocking[A: ACSet, X <: Ob](
    x: X,
    attr: Attr[X, String]
)(v: Elt[X]): Action[A, Unit] =
  for {
    $model <- getModel[A]
    _ <- editTextBlocking(
      Observer(s => $model.update(m => { m.setSubpart(attr, v, s) })),
      $model.now().subpart(attr, v).getOrElse("")
    )
  } yield {}

def dragEntity[A: ACSet, X <: Ob](x: X)(v: Elt[X]): Action[WithProps[A], Unit] =
  for {
    $model <- getModel
    c <- Kleisli.pure($model.now().getProp(v, Center))
    init <- mousePos
    offset <- Kleisli.pure(c - init)
    drag <- Kleisli.ask.map(_.drag)
    _ <- drag.dragStart(
      Observer(p => $model.update(_.setProp(v, Center, p + offset)))
    )
  } yield ()
