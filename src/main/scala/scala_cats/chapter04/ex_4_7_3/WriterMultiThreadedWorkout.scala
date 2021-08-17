package scala_cats.chapter04.ex_4_7_3

import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxWriterId}
import scala_cats.chapter04.WriterMonadWorkout.Logged

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.DurationInt

object WriterMultiThreadedWorkout {
  def slowly[A](body: => A): A =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  def factorial2(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial2(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def main(args: Array[String]): Unit = {
    // Single factorial
    println(factorial(5))

    // Multiple factorials in parallel - the log messages can become interleaved
    println(Await.result(
      Future.sequence(Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )), 5.seconds
    ))

    // Multiple factorials in parallel - the log messages kept separate by use of writer monad
    val result = Await.result(
      Future.sequence(Vector(
        Future(factorial2(5)),
        Future(factorial2(6))
      )), 5.seconds
    )

    result.foreach(w => println(s"Result [${w.value}] Log [${w.written.mkString(", ")}]"))
  }
}