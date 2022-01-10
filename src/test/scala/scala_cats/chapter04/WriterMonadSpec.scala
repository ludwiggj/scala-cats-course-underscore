package scala_cats.chapter04

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala_cats.chapter04.WriterMonad.{Logged, writer1}
import scala_cats.chapter04.ex_4_7_3.WriterMultiThreadedWorkout._
import scala_cats.UnitSpec

class WriterMonadSpec extends UnitSpec {
    "writer monad" can "return results" in {
      import cats.implicits.catsSyntaxWriterId
      val value = 123
      val log = Vector("msg1", "msg2", "msg3")

      val wm = 123.writer(log)

      assert(wm.run == ((log, value)))
      assert(wm.written == log)
      assert(wm.value == value)
    }

    it can "support for comprehension" in {
      assert(writer1.run == ((Vector("a", "b", "c", "x", "y", "z"), 42)))
    }

    it can "transform log via mapWritten" in {
      assert(writer1.mapWritten(_.map(_.toUpperCase)).written == Vector("A", "B", "C", "X", "Y", "Z"))
    }

    it can "transform log and result via bimap" in {
      val writer2 = writer1.bimap(
        log => log.map(_.toUpperCase),
        res => res * 100
      )

      assert(writer2.run == ((Vector("A", "B", "C", "X", "Y", "Z"), 4200)))
    }

    it can "transform log and result via mapBoth" in {
      val writer2 = writer1.mapBoth { (log, res) =>
        val log2 = log.map(_ + "!")
        val res2 = res * 1000
        (log2, res2)
      }

      assert(writer2.run == ((Vector("a!", "b!", "c!", "x!", "y!", "z!"), 42000)))
    }

    it can "reset the log" in {
      val writer2 = writer1.reset

      assert(writer2.run == ((Vector(), 42)))
    }

    it can "swap log and result over" in {
      val writer2 = writer1.swap

      assert(writer2.run == ((42, Vector("a", "b", "c", "x", "y", "z"))))
    }

    private def runParallelFactorials(factorial: Int => Logged[Int]): Vector[(Vector[String], Int)] = {
      Await.result(
        Future.sequence(Vector(
          Future(factorial(5)),
          Future(factorial(6))
        )), 5.seconds
      ).map(_.run)
    }

    it can "keep logs of parallel calculations segregated" in {
      val fact5Result = (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120"), 120)
      val fact6Result = (Vector("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6", "fact 4 24", "fact 5 120", "fact 6 720"), 720)

      val result = Await.result(
        Future.sequence(Vector(
          Future(factorial2(5)),
          Future(factorial2(6))
        )), 5.seconds
      ).map(_.run)

      assert(result == Vector(fact5Result, fact6Result))
      assert(runParallelFactorials(factorial3) == Vector(fact5Result, fact6Result))
      assert(runParallelFactorials(factorial4) == Vector(fact5Result, fact6Result))
      assert(runParallelFactorials(factorial5) == Vector(fact5Result, fact6Result))
      assert(runParallelFactorials(factorial7) == Vector(fact5Result, fact6Result))
    }

    it can "keep logs of parallel calculations segregated but loses all but last log entry" in {
      val fact5Result = (Vector("fact 5 120"), 120)
      val fact6Result = (Vector("fact 6 720"), 720)

      assert(runParallelFactorials(factorial6) == Vector(fact5Result, fact6Result))
    }
}
