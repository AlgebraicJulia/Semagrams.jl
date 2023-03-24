package semagrams.util

import com.raquo.laminar.api.L._

case class UndoState[A](
    recording: Boolean,
    past: List[A],
    present: A,
    future: List[A]
) {
  def undo() = past match {
    case x :: xs => UndoState[A](recording, xs, x, present :: future)
    case Nil     => this
  }

  def redo() = future match {
    case x :: xs => UndoState[A](recording, present :: past, x, xs)
    case Nil     => this
  }

  def save() = this.copy(past = present :: past)

  def update(a: A) = if recording
  then UndoState[A](recording, present :: past, a, Nil)
  else this.copy(present = a, future = Nil)
}

class UndoableVar[A](init: A) extends SignalSource[A] with Sink[A] {
  private val state = Var(UndoState(true, Nil, init, Nil))

  val writer: Observer[A] = state.updater(_.update(_))

  val signal = state.signal.map(_.present)

  def undo() = state.update(_.undo())

  def redo() = state.update(_.redo())

  def record() = state.update(_.copy(recording = true))

  def unrecord() = state.update(_.copy(recording = false))

  def save() = state.update(_.save())

  def updater[B](f: (A, B) => A) =
    state.updater[B]((s, b) => s.update(f(s.present, b)))

  def update(f: A => A) = state.update(s => s.update(f(s.present)))

  def now(): A = state.now().present

  def set(a: A) = state.update(_.update(a))

  def toObservable: Signal[A] = signal

  def toObserver: Observer[A] = writer
}
