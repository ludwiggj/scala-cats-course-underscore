package scala_cats.chapter04

import cats.ApplicativeError
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.catsSyntaxEitherId
import scala_cats.UnitSpec
import scala_cats.chapter04.ApplicativeError.WithThrowable.{badMathException, wasteOfTimeException}
import scala_cats.chapter04.ApplicativeError._

class ApplicativeErrorSpec extends UnitSpec {

  "attemptDivide" can "represent success" in {
    assert(attemptDivide(6, 3) == Right(2))
  }

  it can "represent failure" in {
    assert(attemptDivide(6, 0) == Left(divisorIs0))
  }

  "attemptDivideApplicativeError" can "represent success as an either" in {
    assert(attemptDivideApplicativeError(6, 3) == Right(2))
  }

  it can "represent failure as an either" in {
    assert(attemptDivideApplicativeError(6, 0) == Left(divisorIs0))
  }

  import cats.data.Validated
  type MyValidated[A] = Validated[String, A]

  "attemptDivideApplicativeError" can "represent success as a Validated" in {
    assert(attemptDivideApplicativeError[MyValidated](6, 3) == Valid(2))
  }

  it can "represent failure as a Validated" in {
    assert(attemptDivideApplicativeError[MyValidated](6, 0) == Invalid(divisorIs0))
  }

  "attemptDivideApplicativeErrorWithMap2" can "work as per attemptDivideApplicativeError" in {
    assert(attemptDivideApplicativeErrorWithMap2(6, 3) == Right(2))
    assert(attemptDivideApplicativeErrorWithMap2(6, 0) == Left(divisorIs0))
    assert(attemptDivideApplicativeErrorWithMap2[MyValidated](6, 3) == Valid(2))
    assert(attemptDivideApplicativeErrorWithMap2[MyValidated](6, 0) == Invalid(divisorIs0))
  }

  "attemptDivideApplicativeErrorAbove2" can "divide numbers if denominator is greater than 1" in {
    assert(attemptDivideApplicativeErrorAbove2(6, 0) == Left(badMath))
    assert(attemptDivideApplicativeErrorAbove2(6, 1) == Left(wasteOfTime))
    assert(attemptDivideApplicativeErrorAbove2(6, 2) == Right(3))

    assert(attemptDivideApplicativeErrorAbove2[MyValidated](6, 0) == Invalid(badMath))
    assert(attemptDivideApplicativeErrorAbove2[MyValidated](6, 1) == Invalid(wasteOfTime))
    assert(attemptDivideApplicativeErrorAbove2[MyValidated](6, 2) == Valid(3))
  }

  it can "work with map applicative error" in {
    type MyMap[A] = Map[String, A]

    implicit val x: ApplicativeError[MyMap, String] = new ApplicativeError[MyMap, String] {
      override def raiseError[A](e: String): MyMap[A] = Map(e -> null.asInstanceOf[A])

      override def handleErrorWith[A](fa: MyMap[A])(f: String => MyMap[A]): MyMap[A] = fa

      override def pure[A](x: A): MyMap[A] = Map("result" -> x)

      override def ap[A, B](ff: MyMap[A => B])(fa: MyMap[A]): MyMap[B] = ff.zip(fa).map {
        case ((s, ab), (_, a)) => s -> ab(a)
      }.toMap
    }

    assert(attemptDivideApplicativeErrorAbove2[MyMap](6, 0).keySet == Set(badMath))
    assert(attemptDivideApplicativeErrorAbove2[MyMap](6, 1).keySet == Set(wasteOfTime))
    assert(attemptDivideApplicativeErrorAbove2[MyMap](6, 2) == Map("result" -> 3))
  }

  it can "work with IO applicative error" in {
    import cats.effect.IO

    val value1: IO[Either[Throwable, Int]] = WithThrowable.attemptDivideApplicativeErrorAbove2[IO](6, 0)
    assert(value1.unsafeRunSync() == badMathException.asLeft[Int])
    assert(WithThrowable.attemptDivideApplicativeErrorAbove2[IO](6, 1).unsafeRunSync() == wasteOfTimeException.asLeft[Int])
    assert(WithThrowable.attemptDivideApplicativeErrorAbove2[IO](8, 2).unsafeRunSync() == Right(4))
  }

  "handleAsEither attemptDivideApplicativeErrorAbove2" can "translate results into an either" in {
    assert(handleAsEither(attemptDivideApplicativeErrorAbove2(6, 0)) == Right(badMath.asLeft[Int]))
    assert(handleAsEither(attemptDivideApplicativeErrorAbove2(6, 1)) == Right(wasteOfTime.asLeft[Int]))
    assert(handleAsEither(attemptDivideApplicativeErrorAbove2(10, 2)) == Right(Right(5)))

    assert(handleAsEither(attemptDivideApplicativeErrorAbove2[MyValidated](6, 0)) == Valid(badMath.asLeft[Int]))
    assert(handleAsEither(attemptDivideApplicativeErrorAbove2[MyValidated](6, 1)) == Valid(wasteOfTime.asLeft[Int]))
    assert(handleAsEither(attemptDivideApplicativeErrorAbove2[MyValidated](10, 2)) == Valid(Right(5)))
  }

  "handle attemptDivideApplicativeErrorAbove2" can "translate errors into a value" in {
    assert(handle(attemptDivideApplicativeErrorAbove2(6, 0)) == Right(-1))
    assert(handle(attemptDivideApplicativeErrorAbove2(6, 1)) == Right(-2))
    assert(handle(attemptDivideApplicativeErrorAbove2(10, 2)) == Right(5))

    assert(handle(attemptDivideApplicativeErrorAbove2[MyValidated](6, 0)) == Valid(-1))
    assert(handle(attemptDivideApplicativeErrorAbove2[MyValidated](6, 1)) == Valid(-2))
    assert(handle(attemptDivideApplicativeErrorAbove2[MyValidated](10, 2)) == Valid(5))
  }
}