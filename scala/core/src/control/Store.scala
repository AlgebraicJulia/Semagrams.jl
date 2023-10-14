package semagrams.control

import cats.effect._
import cats.effect.std._
import cats.effect.cps._

import com.raquo.laminar.api.L._

trait Controls[Msg, State] {
  val next: IO[Msg]
  def set(state: State): IO[Unit]
}

trait StoreHandle[Msg, State] {
  val signal: StrictSignal[State]
  val inbox: Observer[Msg]

  val get: IO[State]
  def send(msg: Msg): IO[Unit]
}

trait Store[Msg, State] {
  type Resources

  def run(res: Resources, init: State): IO[StoreHandle[Msg, State]]
}

trait Action[Resources, Msg, State] {
  def apply(res: Resources, init: State, controls: Controls[Msg, State]): IO[Unit]
}


trait AsyncStore[Msg, State] extends Store[Msg, State] {
  val loop: Action[Resources, Msg, State]

  def run(res: Resources, init: State): IO[StoreHandle[Msg, State]] = async[IO] {
    val stateVar = Var(init)
    val queue = Queue.unbounded[IO, Msg].await
    loop(
      res, init,
      new Controls[Msg, State] {
        val next = queue.take
        def set(state: State) = IO(stateVar.set(state))
      }
    ).start.await
    (Dispatcher.sequential[IO] use {
       dispatcher => async[IO] {
         new StoreHandle[Msg, State] {
           val signal = stateVar.signal
           val inbox = Observer[Msg](msg => dispatcher.unsafeRunAndForget(send(msg)))

           val get = IO(stateVar.now())
           def send(msg: Msg) = queue.offer(msg)
         }
       }
     }
    ).await
  }
}

trait PureStore[Msg, State] extends AsyncStore[Msg, State] {
  def process(res: Resources, state: State, msg: Msg): State

  def loop(res: Resources, init: State, controls: Controls[Msg, State]) = async[IO] {
    val msg = controls.next.await
    val nextState = process(res, init, msg)
    controls.set(nextState).await
    loop(res, nextState, controls)
  }
}
