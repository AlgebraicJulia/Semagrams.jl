package semagrams.util

import com.raquo.laminar.api.L._
import com.raquo.airstream.core.Transaction

import cats.effect._
import cats.data.State
import monocle._

extension [A](v: UndoableVar[A]) {
  def updateS_[B](s: State[A, B]): IO[Unit] = {
    IO(v.update(s.run(_).value._1))
  }

  def updateS[B](s: State[A, B]): IO[B] = IO.async_(cb =>
    new Transaction(_ => {
      val a0 = v.now()
      val (a1, b) = s.run(a0).value
      v.set(a1)
      cb(Right(b))
    })
  )

  def zoomL[B](l: Lens[A, B]) = new LensedVar(v, l)
}

class LensedVar[A, B](val v: UndoableVar[A], val l: Lens[A, B]) {
  val writer = v.updater[B]((a, b) => l.replace(b)(a))

  val signal = v.signal.map(l.get)

  def zoomL[C](m: Lens[B, C]) = new LensedVar(v, l.andThen(m))
}
