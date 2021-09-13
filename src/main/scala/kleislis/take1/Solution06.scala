package kleislis.take1

import cats.effect.IO

object Solution06 {
  def main(args: Array[String]): Unit = {
    val r = scala.util.Random

    val generate: Unit => IO[Int] = _ => IO.pure(r.nextInt(100))
    val process: Int => IO[Int] = i => IO.pure((i * math.Pi).toInt)
    val save: Int => IO[Boolean] = i => IO.pure(i > 250)

    val flatMappedVersion: Unit => Boolean = _ =>
      generate(()).flatMap(
        process(_).flatMap(
          save(_)
        )
      ).unsafeRunSync()

    println(s"Result is: ${flatMappedVersion(())}")

    val forCompVersion: Unit => Boolean = _ =>
      (for {
        g <- generate(())
        p <- process(g)
        s <- save(p)
      } yield s).unsafeRunSync()

    println(s"Result is: ${forCompVersion(())}")
  }
}
