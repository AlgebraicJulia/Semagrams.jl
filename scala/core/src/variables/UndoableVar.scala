package semagrams.util

import com.raquo.laminar.api.L._
import cats.effect._
import cats.data.State
import com.raquo.airstream.core.Transaction
import monocle._

/** The internal state of an [[UndoableVar]], implemented as a cursor into a
  * list.
  *
  * @param recording
  *   indicator for whether we should record updates or not
  *
  * @param past
  *   previous states (for undoing)
  *
  * @param present
  *   current state
  *
  * @param future
  *   states that we have undone (for redo). This goes away once you start
  *   editing from a state in the past; we don't have undo trees yet.
  */
case class UndoState[A](
    recording: Boolean,
    past: List[A],
    present: A,
    future: List[A]
) {

  /** Attempt to go one step back into history, remaining the same if there is
    * no history
    */
  def undo() = past match {
    case x :: xs => UndoState[A](recording, xs, x, present :: future)
    case Nil     => this
  }

  /** Attempt to go one step forward into states we have previously undone,
    * remaining the same if no such states exist
    */
  def redo() = future match {
    case x :: xs => UndoState[A](recording, present :: past, x, xs)
    case Nil     => this
  }

  /** Copy the present state onto the undo stack */
  def save() = this.copy(past = present :: past)

  /** Update the state, saving the previous state if `recording` is true */
  def update(a: A) = if recording
  then UndoState[A](recording, present :: past, a, Nil)
  else this.copy(present = a, future = Nil)
}

/** A class similar to `Var` but with support for undo and redo. */
class UndoableVar[A](init: A) extends SignalSource[A] with Sink[A] {
  private val state = Var(UndoState(true, Nil, init, Nil))

  /** An observer that wraps [[UndoState.update]] to record (or not) updates
    * depending on whether `recording` is set in the state
    */
  val writer: Observer[A] = state.updater(_.update(_))

  /** A signal of the current value */
  val signal = state.signal.map(_.present)

  /** Undo the last update */
  def undo() = state.update(_.undo())

  /** Undo the last undo */
  def redo() = state.update(_.redo())

  /** Turn on recording; updates now save previous state in undo history */
  def record() = state.update(_.copy(recording = true))

  /** Turn off recording; updates now do not save previous state */
  def unrecord() = state.update(_.copy(recording = false))

  /** Manually save current state in undo history */
  def save() = state.update(_.save())

  /** Similar to `updater` for `Var`s, but undo-aware */
  def updater[B](f: (A, B) => A) =
    state.updater[B]((s, b) => s.update(f(s.present, b)))

  /** Similar to `update` for `Var`s, but undo-aware */
  def update(f: A => A) = 
    state.update(s => s.update(f(s.present)))

  def updateIO[B](f:A => (A,B)): IO[B] = IO {
    val (a,b) = f(now())
    writer.onNext(a)
    b
  }

  /** Get the current state */
  def now(): A = state.now().present

  /** Set the current state in an undo-aware manner */
  def set(a: A) = state.update(_.update(a))

  /** Coerce to the signal */
  def toObservable: Signal[A] = signal

  /** Coerce to the writer */
  def toObserver: Observer[A] = writer

  /** Use a state monad to update the Var; doesn't return the result.
    *
    * Wraps in IO for convenience.
    */
  def updateS_[B](s: State[A, B]): IO[Unit] = {
    IO(update(s.run(_).value._1))
  }

  /** Use the state monad to update the Var, and returns the result
    * asynchronously after the Var is updated.
    *
    * Note: IO continues before the Transaction ends; is this bad? The reason we
    * want to use a Transaction is so that we don't lose updates to the state in
    * between reading it out and updating it, and we need to return after that
    * Transaction fires asynchronously; this is the only way I know how to do
    * that. If we see weird bugs around this then we can rethink how this works.
    */
  def updateS[B](s: State[A, B]): IO[B] = IO.async_(cb =>
    new Transaction(_ => {
      val a0 = now()
      val (a1, b) = s.run(a0).value
      set(a1)
      cb(Right(b))
    })
  )

  /** Construct a derived variable using `l`; see [[LensedVar]] */
  def zoomL[B](l: Lens[A, B]) = new LensedVar(this, l)
}


object UndoableVar:
  extension [A,B](pair:UndoableVar[(A,B)])
    def updateLeft(f:A => A) = pair.updateBoth(f,b=>b)
    def updateRight(g:B => B) = pair.updateBoth(a=>a,g)
    def updateBoth(f:A=>A,g:B => B) = pair.update((a,b) => (f(a),g(b)))