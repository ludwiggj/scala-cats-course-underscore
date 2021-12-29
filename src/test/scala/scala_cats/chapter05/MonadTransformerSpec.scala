package scala_cats.chapter05

import cats.data.OptionT
import org.scalatest.Assertion
import scala_cats.UnitSpec

class MonadTransformerSpec extends UnitSpec {
  "optionT" can "combine list of options" in {
    import cats.syntax.applicative._ // for pure
    // Book says we also need to import cats.instances.list._, but we don't!

    type ListOption[A] = OptionT[List, A]

    val result1: ListOption[Int] = OptionT(List(Option(10)))
    val result2: ListOption[Int] = 32.pure[ListOption]

    // The map and flatMap methods combine the corresponding methods of List and Option into single operations:
    val result3 = result1.flatMap { x =>
      result2.map { y =>
        x + y
      }
    }

    println(
      s"""
         |OptionT(List(Option(10)) = $result1
         |32.pure[ListOption] = $result2
         |Sum = $result3
         |""".stripMargin)

    val result4 = for {
      r1 <- result1
      r2 <- result2
    } yield r1 + r2

    assert(result3 == 42.pure[ListOption])
    assert(result4.value == List(Some(42)))
  }

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]

  // Build our final monad stack using OptionT:

  // Equivalent to Either[String, Option[A]]
  //    OptionT[ErrorOr, A]
  // => ErrorOr[Option[A]]
  // => Either[String, Option[A]]

  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  it can "combine either of options - both are Right(Some)" in {
    import cats.syntax.applicative._ // for pure
    import cats.syntax.either._      // for asRight

    val a = OptionT[ErrorOr, Int](Right(Some(10)))

    val b = 21.pure[ErrorOrOption]

    val c: OptionT[ErrorOr, Int] = a.flatMap(x => b.map(y => x + y))

    println(
      s"""
         |OptionT[ErrorOr, Int](Right(Some(10))) = $a
         |21.pure[ErrorOrOption] = $b
         |Sum is: $c
         |""".stripMargin
    )

    assert(c == 31.pure[ErrorOrOption])

    val d: ErrorOr[Option[Int]] = c.value                // Either[String, Option[Int]]

    assert(d == Some(31).asRight[String])
    assert(d == 31.pure[ErrorOrOption].value)
    assert(d.map(_.getOrElse(-1)) == 31.asRight[String]) // Either[String, Int]
    assert(d.getOrElse(-1) == Some(31))                  // Option[Int]
  }

  private def checkSuccess(a: OptionT[ErrorOr, Int], b: OptionT[ErrorOr, Int]): Assertion = {
    import cats.syntax.either._      // for asRight

    val c: OptionT[ErrorOr, Int] = a.flatMap(x => b.map(y => x + y))

    println(s"Sum: $a + $b = $c")

    assert(c == b)
    assert(c.value == None.asRight[String])
    assert(c.value.map(_.getOrElse(-1)) == (-1).asRight[String])
    assert(c.value.getOrElse(-1) == None)
  }

  it can "combine either of options - one is None" in {
    import cats.syntax.applicative._ // for pure

    val a: ErrorOrOption[Int] = 10.pure[ErrorOrOption]

    checkSuccess(
      OptionT[ErrorOr, Int](Right(Some(10))),
      OptionT[ErrorOr, Int](Right(None))
    )

//    import cats.implicits._

//    checkSuccess(
//      a,
//      None.pure[ErrorOrOption] // ErrorOrOption[None.type]
//    )

//    checkSuccess(
//      a,
//      none[Int].pure[ErrorOrOption]  // ErrorOrOption[Option[Int]]
//    )

//    checkSuccess(
//      a,
//      OptionT[ErrorOr, Int].pure(None) // None.type
//    )

//    checkSuccess(
//      a,
//      OptionT[ErrorOr, Int].pure(none[Int]) // Option[Int]
//    )

//    checkSuccess(
//      a,
//      OptionT.pure[ErrorOr](None) // OptionT[ErrorOr, None.type]
//    )

//    checkSuccess(
//      a,
//      OptionT.pure[ErrorOr](none[Int])
//    )

    checkSuccess(
      a,
      OptionT.none[ErrorOr, Int]
    )
  }

  private def checkFailure(a: OptionT[ErrorOr, Int],
                           b: OptionT[ErrorOr, Int],
                           errorMsg: String): Assertion = {
    import cats.syntax.either._      // for asRight

    val c: OptionT[ErrorOr, Int] = a.flatMap(x => b.map(y => x + y))

    println(s"Sum: $a + $b = $c")

    assert(c == a)

    val d: ErrorOr[Option[Int]] = c.value                // Either[String, Option[Int]]

    assert(d == errorMsg.asLeft[Option[Int]])
    assert(d.map(_.getOrElse(-1)) == errorMsg.asLeft[Int])
    assert(d.getOrElse(-1) == -1)
  }

  it can "combine failures - 1" in {
    import cats.syntax.applicative._ // for pure

    val errorMsg = "Oh dear"
    val a = OptionT[ErrorOr, Int](Left(errorMsg))
    val b = 21.pure[ErrorOrOption]

    checkFailure(a, b, errorMsg)
  }

  it can "combine failures - 2" in {
    import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId}

    val errorMsg = "Badness"
    val a = errorMsg.raiseError[ErrorOrOption, Int]
    val b = 21.pure[ErrorOrOption]

    checkFailure(a, b, errorMsg)
  }

  "3 monad stack" can "represent future of either of option" in {
    import cats.syntax.applicative._ // for pure
    import cats.syntax.either._      // for asRight
    import scala.concurrent.Future
    import cats.data.{EitherT, OptionT}

    // Outer monad is Future
    // Inner monad is Either
    //   - Error type is String
    //   - Result type is A
    type FutureEither[A] = EitherT[Future, String, A] // Future[Either[String, A]]]

    // Outer monad is FutureEither
    // Inner monad is Option
    //   - Result type is A
    type FutureEitherOption[A] = OptionT[FutureEither, A] // OptionT[EitherT[Future, String, A], A]
                                                          // FutureEither[Option[A]]
                                                          // Future[Either[String, Option[A]]]]

    import cats.instances.future._ // for Monad
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    // Type is OptionT[EitherT[Future, String, Int], Int]
    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 17.pure[FutureEitherOption]
        b <- 26.pure[FutureEitherOption]
      } yield a + b
    println(futureEitherOr)

    // Unpack monad transformers one level at a time

    // Inner type is EitherT[Future, String, Option[Int]]
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