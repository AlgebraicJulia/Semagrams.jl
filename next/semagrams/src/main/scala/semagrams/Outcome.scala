package semagrams

import cats._
import cats.implicits._
import cats.data.WriterT

type Outcome[A] = WriterT[[B] =>> Either[String, B], List[EditorEvent], A]

object Outcome {
  def success[A](v: A): Outcome[A] = WriterT(Right((List(), v)))
}
