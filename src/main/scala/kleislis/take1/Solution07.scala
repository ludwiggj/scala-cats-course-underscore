package kleislis.take1

import cats.data.Kleisli
import cats.effect.IO

object Solution07 {
  def main(args: Array[String]): Unit = {
    val r = scala.util.Random

    val generate: Unit => IO[Int] = _ => IO.pure(r.nextInt(100))
    val process: Int => IO[Int] = i => IO.pure((i * math.Pi).toInt)
    val save: Int => IO[Boolean] = i => IO.pure(i > 250)

    val kleisliCombine_1: Kleisli[IO, Unit, Boolean] = {
      val generateK: Kleisli[IO, Unit, Int] = Kleisli(generate)
      val processK: Kleisli[IO, Int, Int] = Kleisli(process)
      val saveK: Kleisli[IO, Int, Boolean] = Kleisli(save)

      generateK andThen processK andThen saveK
    }

    println(s"Kleilis example 1: ${kleisliCombine_1.run(()).unsafeRunSync()}")

    import cats.syntax.all._
    val kleisliCombine_2: Kleisli[IO, Unit, Boolean] = Kleisli(generate) >>> Kleisli(process) >>> Kleisli(save)

    println(s"Kleilis example 2: ${kleisliCombine_2.run(()).unsafeRunSync()}")

    val kleisliCombine_3: Kleisli[IO, Unit, Boolean] = Kleisli(generate) andThen process andThen save

    println(s"Kleilis example 3: ${kleisliCombine_3.run(()).unsafeRunSync()}")
  }
}
