package kleislis.take2

import cats.data.Kleisli
import cats.effect.IO

object KleisliWorkout {
  def main(args: Array[String]): Unit = {

    val k1: Kleisli[IO, String, Unit] = Kleisli { (context: String) =>
      IO(println(s"printing context: $context"))
    }

    val k2: Kleisli[IO, Unit, Int] = Kleisli.liftF[IO, Unit, Int](IO(1 + 1))

    val k3: Kleisli[IO, Unit, String] = Kleisli.pure("This string is lifted to Kleisli")

    k1.run("Is that you?").unsafeRunSync()
    println(k2.run(()).unsafeRunSync())
    println(k3.run(()).unsafeRunSync())

    println(Kleisli.ask[Option, String].run("Hi"))
    println(Kleisli.ask[IO, String].run("Hi").unsafeRunSync())
  }
}