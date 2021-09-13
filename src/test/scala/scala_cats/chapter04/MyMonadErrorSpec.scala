package scala_cats.chapter04

import cats.implicits.catsSyntaxMonadError
import scala_cats.UnitSpec
import scala_cats.chapter04.MyMonadError.{ErrorOr, catsMonadError, exn, getTemperatureByCoordinates, getTemperatureByCoordinatesAlternate, handleError, handleErrorWith}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Failure

class MyMonadErrorSpec extends UnitSpec {
  // trait MonadError[F[_], E] extends Monad[F]
  // F is the type of the monad;
  // E is the type of error contained within F.
  import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId}

  "monadError" can "represent success" in {
    val success: ErrorOr[Int] = catsMonadError.pure(42)
    assert(success == Right(42))

    assert(42.pure[ErrorOr] == Right(42))

    // Too many clashing imports
    // assert(42.pure == Right(42))
  }

  // Problem with pure statement here:
  // ambiguous implicit values:
  // both method catsMonadErrorForEither in object Invariant of type
  // [A]=> cats.MonadError[[β$0$]scala.util.Either[A,β$0$],A]
  // and method catsInstancesForOption in object Invariant of type
  // => cats.MonadError[Option,Unit] with cats.Alternative[Option] with cats.CoflatMap[Option] with cats.CommutativeMonad[Option]
  // match expected type cats.Applicative[F]
  //     assert(42.pure == Right(42))
  //  it can "represent success with more implicits" in {
  //     assert(42.pure == Right(42))
  //  }

  it can "represent failure" in {
    val failure: ErrorOr[Nothing] = catsMonadError.raiseError("Badness")
    assert(failure == Left("Badness"))

    assert("Badness".raiseError[ErrorOr, Int] == Left("Badness"))
  }

  "monadError handle error with" can "handle the error and recover" in {
    val failure: ErrorOr[Nothing] = catsMonadError.raiseError("Badness")
    assert(handleErrorWith(failure, "It's ok") == Right("It's ok"))

    val failure2: ErrorOr[Int] = "Badness".raiseError[ErrorOr, Int]
    assert(handleErrorWith(failure2, 7) == Right(7))
  }

  it can "handle the error and not recover" in {
    val failure: ErrorOr[Nothing] = catsMonadError.raiseError("Oh no")
    assert(handleErrorWith(failure, "It's ok") == Left("It's not ok"))
  }

  it can "handle success" in {
    val failure: ErrorOr[String] = catsMonadError.pure("Success!")
    assert(handleErrorWith(failure, "It's ok") == Right("Success!"))
  }

  // Problem with pure statement here:
  // ambiguous implicit values:
  // both method catsMonadErrorForEither in object Invariant of type
  // [A]=> cats.MonadError[[β$0$]scala.util.Either[A,β$0$],A]
  // and method catsInstancesForOption in object Invariant of type
  // => cats.MonadError[Option,Unit] with cats.Alternative[Option] with cats.CoflatMap[Option] with cats.CommutativeMonad[Option]
  // match expected type cats.Applicative[F]
  //        256.pure
  //  it can "work with more implicits" in {
  //    val failure = "Badness".raiseError[ErrorOr, Int]
  //    import cats.implicits.catsSyntaxApplicativeError
  //    // failure: ErrorOr[Int] = Left("Badness")
  //    failure.handleErrorWith {
  //      case "Badness" =>
  //        256.pure
  //      case _ =>
  //        "It's not ok".raiseError
  //    }
  //  }

  "monadError handle error" can "handle the error and recover" in {
    val failure: ErrorOr[Nothing] = catsMonadError.raiseError("Badness")
    assert(handleError[Int](failure, 42, 0) == Right(42))

    val failure2: ErrorOr[Int] = "Badness".raiseError[ErrorOr, Int]
    assert(handleError(failure2, 42, 0) == Right(42))

    val failure3: ErrorOr[Int] = "Oh no".raiseError[ErrorOr, Int]
    assert(handleError(failure3, 42, 0) == Right(0))
  }

  it can "handle success" in {
    val failure: ErrorOr[String] = catsMonadError.pure("Success!")
    assert(handleError(failure, "It's ok", "Not ok") == Right("Success!"))
  }

  "monadError ensure" can "return value if predicate satisfied" in {
    assert(catsMonadError.ensure(catsMonadError.pure(42))("Number too low!")(_ < 50) == Right(42))

    assert(42.pure[ErrorOr].ensure("Number too low")(_ < 50) == Right(42))
  }

  it can "return error if predicate not satisfied" in {
    assert(catsMonadError.ensure(catsMonadError.pure(42))("Number too low!")(_ < 40) == Left("Number too low!"))

    assert(42.pure[ErrorOr].ensure("Number too low!")(_ < 40) == Left("Number too low!"))
  }

  "monadError" can "raise error to convert throwable to try" in {
    import scala.util.Try
    assert(exn.raiseError[Try, Int].isInstanceOf[Failure[Int]])
  }

  it can "raise error to convert throwable to future" in {
    the [RuntimeException] thrownBy
      Await.result(exn.raiseError[Future, Int], 1.second) should have message "It's all gone wrong"
  }

  it can "raise error as either with one type parameter fixed" in {
    type EitherWithIntError[A] = Either[Int, A]

    assert(2.raiseError[EitherWithIntError, String] == Left(2))
  }

  type MyEither[A] = Either[String, A]

  "get temperature by co-ordinates" can "return temperature" in {
    assert(getTemperatureByCoordinates[MyEither](44 -> 93) == Right(78))
  }

  "get temperature by co-ordinates alternate" can "return temperature" in {
    assert(getTemperatureByCoordinatesAlternate[MyEither](44 -> 93) == Right(78))
  }

  it can "raise error for invalid coordinate" in {
    assert(getTemperatureByCoordinatesAlternate[MyEither](44 -> -93) == Left("Invalid Coordinates"))
  }

  it can "raise error for (0,0) coordinate" in {
    assert(getTemperatureByCoordinatesAlternate[MyEither](0 -> 0) == Left("(0,0) is at the singularity, dummy!"))
  }
}