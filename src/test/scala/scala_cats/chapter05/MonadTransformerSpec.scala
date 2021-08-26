package scala_cats.chapter05

import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId, catsSyntaxEitherId}
import scala_cats.UnitSpec
import cats.data.OptionT

class MonadTransformerSpec extends UnitSpec {
  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]

  // Build our final monad stack using OptionT:
  // Equivalent to Either[String, Option[A]]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  "list of options" can "combine via optionT" in {
    type ListOption[A] = OptionT[List, A]

    val result1: ListOption[Int] = OptionT(List(Option(10)))
    println(result1)

    val result2: ListOption[Int] = 32.pure[ListOption]
    println(result2)

    val result3 = result1.flatMap { x =>
      result2.map { y =>
        x + y
      }
    }
    println(result3)

    assert(result3 == 42.pure[ListOption])
    assert(result3.value == List(Some(42)))
  }

  "either of options" can "combine via optionT - some success" in {
    val a = OptionT[ErrorOr, Int](Right(Some(10)))
    println(a)

    val b = 21.pure[ErrorOrOption]
    println(b)

    val c = a.flatMap(x => b.map(y => x + y))
    println(c)

    assert(c == 31.pure[ErrorOrOption])
    assert(c.value == Some(31).asRight[String])
    assert(c.value.map(_.getOrElse(-1)) == 31.asRight[String])
    assert(c.value.getOrElse(-1) == Some(31))
  }

  private def checkNoneSuccess(a: OptionT[ErrorOr, Int], b: OptionT[ErrorOr, Int]) = {
    println(a)
    println(b)

    val c = a.flatMap(x => b.map(y => x + y))
    println(c)

    assert(c == b)
    assert(c.value == None.asRight[String])
    assert(c.value.map(_.getOrElse(-1)) == (-1).asRight[String])
    assert(c.value.getOrElse(-1) == None)
  }

  it can "combine via optionT - none success" in {
    val a = OptionT[ErrorOr, Int](Right(Some(10)))
    val b = OptionT[ErrorOr, Int](Right(None))
    checkNoneSuccess(a, b)
  }

  it can "combine via optionT - none success 2" in {
    import cats.implicits._
    val a = OptionT[ErrorOr, Int](Right(Some(10)))

    //    val b = None.pure[ErrorOrOption]
    //    val b = none[Int].pure[ErrorOrOption]
    //    val b = OptionT[ErrorOr, Int].pure(None)
    //    val b = OptionT[ErrorOr, Int].pure(none[Int])
    //    val b = OptionT.pure[ErrorOr](None)
    //    val b = OptionT.pure[ErrorOr](none[Int])
    val b = OptionT.none[ErrorOr, Int]

    checkNoneSuccess(a, b)
  }

  private def checkFailure(a: OptionT[ErrorOr, Int],
                           b: OptionT[ErrorOr, Int],
                           errorMsg: String) = {
    println(a)
    println(b)

    val c = a.flatMap(x => b.map(y => x + y))
    println(c)

    assert(c == a)
    assert(c.value == errorMsg.asLeft[Option[Int]])
    assert(c.value.map(_.getOrElse(-1)) == errorMsg.asLeft[Int])
    assert(c.value.getOrElse(-1) == -1)
  }

  it can "combine via optionT - failure 1" in {
    val errorMsg = "Oh dear"
    val a = OptionT[ErrorOr, Int](Left(errorMsg))
    val b = 21.pure[ErrorOrOption]

    checkFailure(a, b, errorMsg)
  }

  it can "combine via optionT - failure 2" in {
    val errorMsg = "Badness"
    val a = errorMsg.raiseError[ErrorOrOption, Int]
    val b = 21.pure[ErrorOrOption]

    checkFailure(a, b, errorMsg)
  }

  "3 monad stack" can "represent future of either of option" in {
    import scala.concurrent.Future
    import cats.data.{EitherT, OptionT}

    // Outer monad is Future
    // Inner monad is Either
    //   - Error type is String
    //   - Result type is A
    type FutureEither[A] = EitherT[Future, String, A]

    // Outer monad is FutureEither
    // Inner monad is Option
    //   - Result type is A
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import cats.instances.future._ // for Monad
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    // Type is OptionT[EitherT[Future, String, Int], Int]
    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 17.pure[FutureEitherOption]
        b <- 26.pure[FutureEitherOption]
      } yield (a + b)
    println(futureEitherOr)

    // Type is EitherT[Future, String, Option[Int]]
    val intermediate: FutureEither[Option[Int]] = futureEitherOr.value
    println(intermediate)

    // Type is Future[Either[String, Option[Int]]]
    val stack: Future[Either[String, Option[Int]]] = intermediate.value
    println(stack)

    val result = Await.result(stack, 1.second)
    println(result)

    assert(result == Some(43).asRight[String])
  }
}