package semagrams.util

import com.raquo.laminar.api.L._
import monocle._
import com.raquo.airstream.core.Observer
import com.raquo.airstream.core.Signal

/** A view of a variable. Uses a lens to forward gets and sets to an underlying
  * `UndoableVar`.
  *
  * Note: this is specialized to `UndoableVar` right now because there isn't an
  * informative superclass of both `UndoableVar` and `Var`. At some point we
  * should fix this.
  *
  * Maybe we should send a PR with `UndoableVar` and `LensedVar` to Laminar?
  *
  * @todo
  *   implement more of the `Var` api.
  */
class LensedVar[A, B](val v: UndoableVar[A], val l: Lens[A, B]) extends WeakVar[B] {

  /** An observer that uses the lens to write to the underlying var. */

  def toObservable: Signal[B] = v.toObservable.map(l.get)

  def toObserver: Observer[B] = writer

  val writer: Observer[B] = v.updater[B]((a, b) => l.replace(b)(a))

  /** A signal that uses the getter of the lens to output a derived signal */
  val signal: Signal[B] = v.signal.map(l.get)

  def now() = l.get(v.now())

  def set(b:B) = v.update(l.replace(b))

  def update(f:B => B) = v.update(l.replace(f(now())))

  // Pause recording for base variable? This seems bad.
  def unrecord() = v.unrecord()
  def record() = v.record()
  def save() = v.save()

  /** Construct a further LensedVar by composing lenses */
  def zoomL[C](m: Lens[B, C]) = new LensedVar(v, l.andThen(m))
}
