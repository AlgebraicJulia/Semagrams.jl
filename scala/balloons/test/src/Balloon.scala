package balloons

import weaver._
import cats.effect.cps._
import cats.effect._

enum CounterMsg {
  case Inc
  case Dec
  case Reverso
}

import CounterMsg._

case class Counter(state: Int) extends PureBalloon[CounterMsg, Int] {
  def current = state

  def next(msg: CounterMsg) = msg match {
    case Inc     => Counter(state + 1)
    case Dec     => Counter(state - 1)
    case Reverso => ReverseCounter(state)
  }
}

case class ReverseCounter(state: Int) extends PureBalloon[CounterMsg, Int] {
  def current = state

  def next(msg: CounterMsg) = msg match {
    case Inc     => ReverseCounter(state - 1)
    case Dec     => ReverseCounter(state + 1)
    case Reverso => Counter(state)
  }
}

def reverso(helm: Helm[CounterMsg, Int]): IO[Unit] = async[IO] {
  val (msg, state, update) = helm.nextWithState.await
  msg match {
    case Inc => update(state - 1).await
    case Dec => update(state + 1).await
    case Reverso => {
      update(state).await
      reverso(helm).await
    }
  }
}

/** Just apply reverso for one round when we get a reverso message */
def processCounter(helm: Helm[CounterMsg, Int]): IO[Void] =
  async[IO] {
    val (msg, state, update) = helm.nextWithState.await
    msg match {
      case Inc => {
        update(state + 1).await
      }
      case Dec => {
        update(state - 1).await
      }
      case Reverso => {
        update(state).await
        reverso(helm).await
      }
    }
    processCounter(helm).await
  }

object BalloonSuite extends SimpleIOSuite {
  test("basic counter") {
    async[IO] {
      val tether = Counter(0).launch.await
      val s0 = tether.get.await
      tether.send(Inc).await
      val s1 = tether.get.await
      tether.send(Dec).await
      val s2 = tether.get.await
      expect.all(s0 == 0, s1 == 1, s2 == 0)
    }
  }

  test("reverso counter") {
    async[IO] {
      val tether = Counter(0).launch.await
      tether.send(Inc).await
      tether.send(Reverso).await
      tether.send(Inc).await
      val s = tether.get.await
      expect(s == 0)
    }
  }

  test("basic counter functional") {
    async[IO] {
      val tether = AsyncBalloon(processCounter, 0).launch.await
      val s0 = tether.get.await
      tether.send(Inc).await
      val s1 = tether.get.await
      tether.send(Dec).await
      val s2 = tether.get.await
      expect.all(s0 == 0, s1 == 1, s2 == 0)
    }
  }

  test("reverso counter functional") {
    async[IO] {
      val tether = AsyncBalloon(processCounter, 0).launch.await
      tether.send(Inc).await
      tether.send(Reverso).await
      tether.send(Inc).await
      val s = tether.get.await
      expect(s == 0)
    }
  }
}
