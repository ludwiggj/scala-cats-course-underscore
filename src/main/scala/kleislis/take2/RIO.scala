package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO

object RIO {
  type RIO[A] = Kleisli[IO, CorrelationId, A]

  def unit: RIO[Unit] = Kleisli.pure(())

  def liftF[B](x: IO[B]): RIO[B] = Kleisli.liftF[IO, CorrelationId, B](x)

  def pure[B](x: B): RIO[B] = Kleisli.pure(x)

  def apply[B](f: CorrelationId => IO[B]): RIO[B] = Kleisli(f)

  def ask: RIO[CorrelationId] = Kleisli.ask[IO, CorrelationId]
}
