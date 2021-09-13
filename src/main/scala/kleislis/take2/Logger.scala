package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO
import kleislis.take2.Logger.CorrelationId

trait Logger {
  def info(msg: String): Kleisli[IO, CorrelationId, Unit] = Kleisli { cid =>
    IO(println(s"[$cid] > $msg"))
  }
}

object Logger {
  type CorrelationId = String
}