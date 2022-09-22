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

def addEntityPos[A: ACSet, X <: Ob](x: X): Action[WithProps[A], Unit] = for {
  pos <- mousePos
  _ <- updateModelS[WithProps[A], Elt[X]](
    addPartWP(x, PropMap() + (Center, pos) + (Content, ""))
  )
  _ <- update
} yield ()

def remEntity[A: ACSet]: Action[A, Unit] = for {
  v <- fromMaybe(hovered)
  _ <- updateModel[A](_.remPart(v))
  _ <- update
} yield ()

def dragEdge[A: ACSet, X <: Ob, Y <: Ob, Z <: Ob](
    src: Hom[X, Y],
    tgt: Hom[X, Z],
    key: String,
)(implicit withPropsACSet: ACSet[WithProps[A]]): Action[WithProps[A], Unit] = {
  val L = actionLiftIO[WithProps[A]]
  val mainAction: Action[WithProps[A], Unit] = for {
    drag <- Kleisli.ask.map(_.drag)
    $model <- Kleisli.ask.map(_.$model)
    s <- fromMaybe(
      Bindings(clickOn(ClickType.Single, MouseButton.Left, src.codom.asInstanceOf[Y])).run
    )
    p <- mousePos
    e <- updateModelS[WithProps[A], Elt[X]](for {
      e <- addPartWP(src.dom.asInstanceOf[X], PropMap())
      _ <- setSubpart(src, e, s)
      _ <- setProp(e, End, p)
    } yield e)
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setProp(e, End, p))))
      t <- fromMaybe(hoveredPart(tgt.codom.asInstanceOf[Z]))
      _ <- updateModelS[WithProps[A], Unit](setSubpart(tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- L.liftIO(IO(drag.$state.set(None)))
      _ <- updateModelS[WithProps[A], Unit](remPart(e))
    } yield ())
    _ <- update
  } yield ()

  for {
    drag <- Kleisli.ask.map(_.drag)
    target <- mainAction.forever.start
    _ <- Bindings(keyUp(key)).run
    _ <- Kleisli(_ => target.cancel)
  } yield {}
}

def editContent[A: ACSet, X <: Ob](x: X)(v: Elt[X]): Action[WithProps[A], Unit] =
  for {
    $model <- getModel[WithProps[A]]
    _ <- editText(
      Observer(s => $model.update(m => { m.setProp(v, Content, s) })),
      $model.now().getProp(v, Content)
    )
  } yield {}

def dragEntity[A: ACSet, X <: Ob](x: X)(v: Elt[X]): Action[WithProps[A], Unit] = for {
  $model <- getModel
  c <- Kleisli.pure($model.now().getProp(v, Center))
  init <- mousePos
  offset <- Kleisli.pure(c - init)
  drag <- Kleisli.ask.map(_.drag)
  _ <- drag.dragStart(
    Observer(p => $model.update(_.setProp(v, Center, p + offset)))
  )
} yield ()
