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

  def factorial3(n: Logged[Int]): Logged[Int] = {
    for {
      ans <- if (n.value == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial3(n.map(_ - 1)).map(_ * n.value))
      }
      _ <- Vector(s"fact ${n.value} $ans").tell
    } yield ans
  }

  def factorial4(n: Logged[Int]): Logged[Int] = {
    val ans = if (n.value == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial4(n.map(_ - 1)).map(_ * n.value))
    }
    for {
      a <- ans
      _ <- Vector(s"fact ${n.value} ${ans.value}").tell
    } yield a
  }

  def factorial5(n: Logged[Int]): Logged[Int] = {
    val ans = if (n.value == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial4(n.map(_ - 1)).map(_ * n.value))
    }
    ans.flatMap(a => Vector(s"fact ${n.value} ${ans.value}").tell.map(_ => a))
  }

  def factorial6(n: Logged[Int]): Logged[Int] = {
    val ans = if (n.value == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial4(n.map(_ - 1)).map(_ * n.value))
    }
    ans.mapWritten(_ ++ Vector(s"fact ${n.value} ${ans.value}"))
  }

  def factorial7(n: Logged[Int]): Logged[Int] = {
    val ans = if (n.value == 0) {
      1.pure[Logged]
    } else {
      slowly(factorial4(n.map(_ - 1)).map(_ * n.value))
    }
    ans.mapWritten(_ => Vector(s"fact ${n.value} ${ans.value}"))
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
  }
}