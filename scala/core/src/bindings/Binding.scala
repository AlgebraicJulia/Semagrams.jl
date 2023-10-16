package semagrams.bindings

import cats.effect._
import cats._
import semagrams._

/** A pair of a compatible [[EventHook]] and [[Action]]. As each of these have
  * descriptions, an automatic help text can be generated for the binding.
  */
trait Binding[Model] {

  override def toString = s"Binding($hook,$action)"
  type EventData

  val hook: EventHook[EventData]

  val action: Action[EventData, Model]
}

object Binding {

  /** Construct a new anonymous binding from an [[EventHook]] and [[Action]]
    */
  def apply[A, B](h: EventHook[A], a: Action[A, B]): Binding[B] =
    new Binding[B] {
      type EventData = A
      val hook = h
      val action = a
    }

  /** Run the first binding that matches an event.
    *
    * @param evt
    *   The event to match against
    *
    * @param bindings
    *   The list of bindings to try to match the evt against
    */
  def process[Model](
      evt: Event,
      r: Action.Resources[Model],
      bindings: Seq[Binding[Model]]
  ): IO[Unit] = 
    import cats.implicits._
    for {
    globalState <- IO(r.globalStateVar.now())

    _ = evt match
      case KeyDown(k) =>
        println(s"key = $k")
        println(r.stateVar.now().hovered)
      case _ => ()
    
    _ <- bindings.flatMap{ (b:Binding[Model]) =>
      b.hook(evt,globalState)
        .map(b.action(_,r))
    }.parSequence_
  } yield ()

  def processAll[Model](r: Action.Resources[Model], bindings: Seq[Binding[Model]]): IO[Unit] = {
    Monad[IO].whileM_(IO(true)) {
      for {
        evt <- r.eventQueue.take
        _ = evt match
          case _ => ()        
        _ <- r.processEvent(evt)
        _ = evt match
          case _ => ()
        _ <- Binding.process(evt, r, bindings)
        _ = evt match
          case _ => ()
      } yield ()
    }
  }
}
