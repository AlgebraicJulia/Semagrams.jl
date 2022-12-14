package semagrams.util

import com.raquo.laminar.api.L._
import com.raquo.airstream.core.Transaction

import cats.effect._
import cats.data.State

extension [A](v: Var[A]) {
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
}
