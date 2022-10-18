package semagrams.actions

import com.raquo.laminar.api.L._
import semagrams._
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

def addPartPos[S: IsSchema](
    ob: Ob,
    props: PropMap
): Action[ACSet[S], Part] =
  mousePos.flatMap(p =>
    updateModelS(ACSetOps[S].addPart(ob, props + (Center, p)))
  )

def remPart[S: IsSchema]: Action[ACSet[S], Unit] = for {
  v <- fromMaybe(hovered)
  _ <- v match {
    case (p: Part) => updateModel[ACSet[S]](_.remPart(p))
    case _         => ops.pure(())
  }
  _ <- update
} yield ()

def hoveredPart[M](ob: Ob) =
  hoveredEntity[M](ob).map(_.map(_.asInstanceOf[Part]))

def dragEdge[S: IsSchema](
    ob: Ob,
    src: Hom,
    tgt: Hom,
    s: Part
): Action[ACSet[S], Unit] = {
  val aops = summon[ACSetOps[S]]
  for {
    drag <- ops[ACSet[S]].ask.map(_.drag)
    $model <- ops[ACSet[S]].ask.map(_.$model)
    p <- mousePos
    e <- updateModelS(aops.addPart(ob, PropMap().set(src, s).set(End, p)))
    _ <- (for {
      _ <- drag.drag(Observer(p => $model.update(_.setSubpart(End, e, p))))
      t <- fromMaybe(hoveredPart[ACSet[S]](tgt.codom))
      _ <- updateModelS(aops.setSubpart(tgt, e, t))
    } yield ()).onCancelOrError(for {
      _ <- ops.delay(drag.$state.set(None))
      _ <- updateModelS(aops.remPart(e))
    } yield ())
    _ <- update
  } yield ()
}

def editStringProp[S: IsSchema](
    attr: Property { type Value = String }
)(v: Part): Action[ACSet[S], Unit] =
  for {
    $model <- getModel[ACSet[S]]
    _ <- editText(
      Observer(s => $model.update(m => { m.setSubpart(attr, v, s) })),
      $model.now().trySubpart(attr, v).getOrElse("")
    )
  } yield {}

def dragPart[S: IsSchema](v: Part): Action[ACSet[S], Unit] =
  for {
    $model <- getModel
    c <- Kleisli.pure($model.now().subpart(Center, v))
    init <- mousePos
    offset <- Kleisli.pure(c - init)
    drag <- Kleisli.ask.map(_.drag)
    _ <- drag.dragStart(
      Observer(p => $model.update(_.setSubpart(Center, v, p + offset)))
    )
  } yield ()
