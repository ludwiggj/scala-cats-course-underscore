package scala_cats.chapter03

import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

object FunctorWorkout {
  def main(args: Array[String]): Unit = {
    val future: Future[String] =
      Future(123).
        map(n => n + 1).
        map(n => n * 2).
        map(n => s"$n!")
    println(Await.result(future, 1.second))

    val future1 = {
      // Initialize Random with a fixed seed:
      val r = new Random(0L)
      // nextInt has the side-effect of moving to
      // the next random number in the sequence:
      val x = Future(r.nextInt)
      for {
        a <- x
        b <- x
      } yield (a, b)
    }

    val future2 = {
      val r = new Random(0L)
      for {
        a <- Future(r.nextInt)
        b <- Future(r.nextInt)
      } yield (a, b)
    }
    println(Await.result(future1, 1.second))
    println(Await.result(future2, 1.second))
  }
}
