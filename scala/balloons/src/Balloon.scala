package balloons

import cats.effect._
import com.raquo.laminar.api.L._
import cats.effect.std._
import cats.effect.cps._

trait Cable[Msg, State] {
  def signal: StrictSignal[State]
  def inbox: Observer[Msg]

  def get: IO[State]
  def send(msg: Msg): IO[Unit]
}

trait Helm[Msg, State] {
  def next: IO[(Msg, State => IO[Unit])]

  def nextWithState: IO[(Msg, State, State => IO[Unit])]

  def process[B](handler: Msg => IO[(B, State)]): IO[B] = async[IO] {
    val (msg, update) = next.await
    val (b, state) = handler(msg).await
    update(state).await
    b
  }

  def process_[B](handler: Msg => (B, State)): IO[B] =
    process(msg => IO(handler(msg)))
}

object Balloon {
  def launch[Msg, State](
      runner: Helm[Msg, State] => IO[Any],
      init: State
  ): IO[Cable[Msg, State]] = async[IO] {
    val stateVar = Var(init)
    val stateCell = AtomicCell[IO].of(init).await
    val queue = Queue.unbounded[IO, (Msg, State => IO[Unit])].await
    runner(new Helm {
      def nextWithState = async[IO] {
        val (msg, update) = next.await
        val state = stateCell.get.await
        (msg, state, update)
      }
      def next = queue.take
    }).start.await
    (Dispatcher.sequential[IO] use { dispatcher =>
      IO(new Cable[Msg, State] {
        val signal = stateVar.signal
        val inbox =
          Observer[Msg](msg => dispatcher.unsafeRunAndForget(sendAsync(msg)))

        private def set(state: State) = async[IO] {
          stateCell.set(state).await
          stateVar.set(state)
        }

        val get = stateCell.get

        def send(msg: Msg) = async[IO] {
          val hole = Deferred[IO, State].await
          queue.offer((msg, state => hole.complete(state) >> IO(()))).await
          set(hole.get.await).await
        }

        def sendAsync(msg: Msg): IO[Unit] = queue.offer((msg, set))
      })
    }).await
  }
}

/** Pure state machines can be more efficient than using Helm, but can also
  * interoperate with it
  */
trait PureBalloon[Msg, State] {
  def current: State

  def next(msg: Msg): PureBalloon[Msg, State]

  def step(helm: Helm[Msg, State]): IO[PureBalloon[Msg, State]] =
    helm.process_(msg => { val b = next(msg); (b, b.current) })

  def launch: IO[Cable[Msg, State]] = async[IO] {
    val stateVar = Var(current)
    val stateCell = AtomicCell[IO].of(this).await
    (Dispatcher.sequential[IO] use { dispatcher =>
      IO(new Cable[Msg, State] {
        val signal = stateVar.signal
        val inbox =
          Observer[Msg](msg => dispatcher.unsafeRunAndForget(send(msg)))

        val get = stateCell.get.map(_.current)
        def send(msg: Msg) = async[IO] {
          val nextBalloon = stateCell
            .modify(balloon => {
              val nextBalloon = balloon.next(msg)
              (nextBalloon, nextBalloon)
            })
            .await
          stateVar.set(nextBalloon.current)
        }
      })
    }).await
  }
}
