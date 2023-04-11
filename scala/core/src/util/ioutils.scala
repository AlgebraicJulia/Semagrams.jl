package semagrams.util

import cats._
import cats.effect._
import upickle.default._

case object NoneError extends Exception

extension [A](x: Option[A]) {

  /** Get the value of an option, throwing an error in a monad if the option is
    * None. The advantage of throwing the error in the monad is that its more
    * easily catchable.
    */
  def unwrap[F[_]](implicit F: MonadError[F, Throwable]): F[A] = {
    x match {
      case Some(x) => F.pure(x)
      case None    => F.raiseError(NoneError)
    }
  }
}

/** A convenience wrapper around calling [[unwrap]] for IO */
def fromMaybe[Model, A](a: IO[Option[A]]): IO[A] =
  a.flatMap(_.unwrap)


def msgError(msg:String): RuntimeException = {
  println(msg)
  new RuntimeException
}

def bijectiveRW[A](as: Iterable[A]): ReadWriter[A] = readwriter[String].bimap(
  a => a.toString,
  str => as.find(a => str == a.toString).getOrElse(
    throw msgError(s"bad bijectiveRW: $str not in $as")
  )
)
extension [A](action: IO[A]) {

  /** Convert a possibly erroring IO action to one that safely returns an Option
    */
  def toOption: IO[Option[A]] =
    action.map(Some(_)).handleError(_ => None)

  /** Perform `fin` if the action is cancelled or errors.
    *
    * Note that if the action is cancelled, it never returns, so we only get the
    * result of `fin` if it errors.
    */
  def onCancelOrError(fin: IO[A]): IO[A] =
    action.onCancel(fin.map(_ => ())).handleErrorWith(_ => fin)
}
