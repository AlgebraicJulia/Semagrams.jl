package semagrams.util

import cats._
import cats.effect._

case object NoneError extends Exception

extension [A](x: Option[A]) {
  def unwrap[F[_]](implicit F: MonadError[F, Throwable]): F[A] = {
    x match {
      case Some(x) => F.pure(x)
      case None    => F.raiseError(NoneError)
    }
  }
}

def fromMaybe[Model, A](a: IO[Option[A]]): IO[A] =
  a.flatMap(_.unwrap)

extension [A](action: IO[A]) {
  def toOption: IO[Option[A]] =
    action.map(Some(_)).handleError(_ => None)

  def onCancelOrError(fin: IO[A]) =
    action.onCancel(fin.map(_ => ())).handleErrorWith(_ => fin)
}
