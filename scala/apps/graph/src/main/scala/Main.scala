package graph

import semagrams.*
import semagrams.actions.*
import cats.Monad
import cats.effect.IO

object Main {
  def main(args: Array[String]): Unit = {
    val L = actionLiftIO[Unit]
    val action = for {
      _ <- L.liftIO(IO.println("Hello World"))
    } yield ()

    mountWithAction("app-container", (), action)
  }
}
