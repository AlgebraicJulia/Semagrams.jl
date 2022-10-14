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

def addEntityPos[A: HasProps, X <: Ob](
    x: X,
    init: Elt[X] => State[A, Unit]
): Action[A, Elt[X]] =
  for {
    pos <- mousePos
    v <- updateModelS[A, Elt[X]](
      for {
        v <- HasProps.ops.addPartWP(x, PropMap() + (Center, pos))
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

def dragEdge[A: HasProps, X <: Ob, Y <: Ob, Z <: Ob](
    src: Hom[X, Y],
    tgt: Hom[X, Z],
    s: Elt[Y]
)(implicit withPropsACSet: ACSet[A]): Action[A, Unit] = {
  for {
    drag <- ops[A].ask.map(_.drag)
    $model <- ops[A].ask.map(_.$model)
    p <- mousePos
    e <- updateModelS[A, Elt[X]](for {
      e <- HasProps.ops.addPartWP(src.dom, PropMap() + (End, p))
      _ <- HasProps.ops.setSubpart(src, e, s)
    } yield e)
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setProp(e, End, p))))
      t <- fromMaybe(hoveredPart(tgt.codom.asInstanceOf[Z]))
      _ <- updateModelS(HasProps.ops.setSubpart(tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- ops.delay(drag.$state.set(None))
      _ <- updateModelS(HasProps.ops.remPart(e))
    } yield ())
    _ <- update
  } yield ()
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
