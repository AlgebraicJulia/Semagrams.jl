package control

case class Transducer[Msg, Out](next: Msg => (Transducer[Msg, Out], Out))
