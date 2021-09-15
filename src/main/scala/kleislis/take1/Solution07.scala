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

      // def andThen[C](f: B => F[C])(implicit F: FlatMap[F]): Kleisli[F, A, C]
      // def andThen[C](f: Kleisli[F, B, C])(implicit F: FlatMap[F]): Kleisli[F, A, C]
      // def flatMap[C, AA <: A](f: B => Kleisli[F, AA, C])(implicit F: FlatMap[F]): Kleisli[F, AA, C]
      generateK andThen processK andThen saveK
    }

    println(s"Kleilis example 1: ${kleisliCombine_1.run(()).unsafeRunSync()}")

    import cats.syntax.all._
    val kleisliCombine_2: Kleisli[IO, Unit, Boolean] = Kleisli(generate) >>> Kleisli(process) >>> Kleisli(save)

    println(s"Kleilis example 2: ${kleisliCombine_2.run(()).unsafeRunSync()}")

    val kleisliCombine_3: Kleisli[IO, Unit, Boolean] = Kleisli(generate) andThen process andThen save

    println(s"Kleilis example 3: ${kleisliCombine_3.run(()).unsafeRunSync()}")

    val kleisliCombine_4: Kleisli[IO, Unit, Boolean] = {
      val generate2: Unit => IO[Int] = _ => IO.pure(r.nextInt(100))
      val process2: Int => IO[String] = i => IO.pure(i.toString)
      val save2: String => IO[Boolean] = i => IO.pure(i.toInt > 50)

      val generateK: Kleisli[IO, Unit, Int] = Kleisli(generate2)
      val processK: Kleisli[IO, Int, String] = Kleisli(process2)
      val saveK: Kleisli[IO, String, Boolean] = Kleisli(save2)

      generateK andThen processK andThen saveK
    }

    println(s"Kleilis example 4: ${kleisliCombine_4.run(()).unsafeRunSync()}")
  }
}
