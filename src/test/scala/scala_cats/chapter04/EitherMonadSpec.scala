package scala_cats.chapter04

import cats.implicits.catsSyntaxEitherId
import cats.syntax.either._
import scala.util.Try
import EitherMonad.countPositive
import scala_cats.UnitSpec

class EitherMonadSpec extends UnitSpec {
  "either monad" can "make either nicer" in {
    val a: Either[String, Int] = 3.asRight[String]
    val b: Either[String, Int] = 4.asRight[String]

    val res = for {
      x <- a
      y <- b
    } yield x * x + y * y

    assert(res === Right(25))
  }

  it can "count number of positive elements" in {
    assert(countPositive(List(1, 2, 3)) == Right(3))
  }

  it can "abort count number of positive elements if an element is negative" in {
    assert(countPositive(List(1, -2, 3)) == Left("Negative number - abort!"))
  }

  it can "catch exceptions" in {
    val expected = """java.lang.NumberFormatException: For input string: "foo""""
    assert(Either.catchOnly[NumberFormatException]("foo".toInt).leftMap(_.toString) == Left(expected))
  }

  it can "catch non-fatal errors" in {
    val expected = "java.lang.RuntimeException: Badness"
    assert(Either.catchNonFatal(sys.error("Badness")).leftMap(_.toString) == Left(expected))
  }

  it can "convert try to either" in {
    val expected = """java.lang.NumberFormatException: For input string: "foo""""
    assert(Either.fromTry(Try("foo".toInt)).leftMap(_.toString) == Left(expected))
  }

  it can "convert option to either" in {
    assert(Either.fromOption[String, Int](Some(5), "Badness") == Right(5))
    println(Either.fromOption[String, Int](None, "Badness") == Left("Badness"))
  }

  it can "get value or default it" in {
    assert("Error".asLeft[Int].getOrElse(0) == 0)
    assert(2.asRight[String].getOrElse(0) == 2)
  }

  it can "get value or default it (2)" in {
    assert("Error".asLeft[Int].orElse(0.asRight[String]) == 0.asRight[String])
    assert(2.asRight[String].orElse(0.asRight[String]) == 2.asRight[String])
    assert(Right(2).orElse(0.asRight[String]) == Right(2))
  }

  it can "check condition against right" in {
    assert((-1).asRight[String].ensure("Must be non-negative")(_ > 0) == Left("Must be non-negative"))
    assert(Right(2).ensure("Must be non-negative")(_ > 0) == Right(2))
    assert("Err".asLeft[Int].ensure("Must be non-negative")(_ > 0) == Left("Err"))
  }

  it can "Recover errors" in {
    assert("error".asLeft[Int].recover { case _: String => -1 } == Right(-1))
    assert("error".asLeft[Int].recoverWith { case _: String => Right(-1) } == Right(-1))
  }

  it can "left map" in {
    assert("foo".asLeft[Int].leftMap(_.reverse) == Left("oof"))
    assert("bar".asRight[String].leftMap(_.reverse) == Right("bar"))
  }

  it can "bimap" in {
    assert(6.asRight[String].bimap(_.reverse, _ * 7) == Right(42))
    assert("foo".asLeft[Int].bimap(_.reverse, _ * 7) == Left("oof"))
  }

  it can "swap" in {
    val e: Either[String, Int] = 123.asRight[String]

    val swapped: Either[Int, String] = e.swap

    assert(swapped == Left(123))
  }
}