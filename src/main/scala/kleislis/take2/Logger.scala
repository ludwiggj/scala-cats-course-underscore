package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO

trait Logger {
  def info(msg: String): Kleisli[IO, CorrelationId, Unit] = Kleisli { cid =>
    IO(println(s"[${cid.id}] > $msg"))
  }
}

case class CorrelationId(id: String)