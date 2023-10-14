package acsets

import upickle.default._

trait Property {
  type Val

  val rw: ReadWriter[Val]
}

trait FinSet[A] {
  type T <: A

  val rw: ReadWriter[A]

  def all: Seq[A]
}
