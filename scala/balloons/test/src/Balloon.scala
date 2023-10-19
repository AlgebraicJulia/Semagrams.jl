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

object BalloonSuite extends SimpleIOSuite {
  test("basic counter") {
    async[IO] {
      val cable = Counter(0).run.await
      val s0 = cable.get.await
      cable.send(Inc).await
      val s1 = cable.get.await
      cable.send(Dec).await
      val s2 = cable.get.await
      expect.all(s0 == 0, s1 == 1, s2 == 0)
    }
  }

  test("reverso counter") {
    async[IO] {
      val cable = Counter(0).run.await
      cable.send(Inc).await
      cable.send(Reverso).await
      cable.send(Inc).await
      val s = cable.get.await
      expect(s == 0)
    }
  }
}
