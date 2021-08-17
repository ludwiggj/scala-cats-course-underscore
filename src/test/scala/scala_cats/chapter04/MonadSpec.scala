package scala_cats.chapter04

import cats.implicits.{catsKernelStdOrderForInt, catsSyntaxWriterId}
import scala_cats.UnitSpec
import cats.{Eq, Monad}
import cats.syntax.either._
import scala_cats.chapter04.WriterMonadWorkout.Logged
import scala_cats.chapter04.ex_4_5_4.Validate.{illegalAgeException, validateAdult}
import scala_cats.chapter04.ex_4_6_5.FoldEval.{foldRightSafe, foldRightTextbook}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

//noinspection OptionEqualsSome
class MonadSpec extends UnitSpec {

  "cats monad" can "supports option" in {
    val opt1 = Monad[Option].pure(3)
    assert(opt1 === Some(3))

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    assert(opt2 === Some(5))

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    assert(opt3 === Some(500))

    assert(Monad[Option].flatMap(Option(1))(a => Option(a * 2)) === Some(2))
  }

  it can "support list" in {
    val list1 = Monad[List].pure(3)
    assert(list1 === List(3))

    val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    assert(list2 === List(1, 10, 2, 20, 3, 30))

    val list3 = Monad[List].map(list2)(a => a + 123)
    assert(list3 === List(124, 133, 125, 143, 126, 153))
  }

  it can "support vector" in {
    assert(Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10)) === Vector(1, 10, 2, 20, 3, 30))
  }

  it can "support future" in {
    import scala.concurrent.duration.DurationInt
    import scala.concurrent.ExecutionContext.Implicits.global
    val fm = Monad[Future]
    val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

    assert(Await.result(future, 1.second) === 3)
  }

  "pure" can "create appropriate types" in {
    import cats.syntax.applicative._

    assert(1.pure[Option] === Some(1))

    assert(1.pure[List] === List(1))
  }

  object MonadOps {

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    def sumSquareDesugared[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
      a.flatMap(x => b.map(y => x * x + y * y))
    }

    def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = {
      for {
        x <- a
        y <- b
      } yield x * x + y * y
    }
  }

  "sumSquare" can "add up sum of squares" in {
    import MonadOps.sumSquareDesugared
    assert(sumSquareDesugared(Option(3), Option(4)) === Some(25))
    assert(sumSquareDesugared(List(1, 2, 3), List(4, 5)) === List(17, 26, 20, 29, 25, 34))
  }

  it can "be expressed by a for comprehension" in {
    import MonadOps.sumSquare
    assert(sumSquare(Option(3), Option(4)) === Some(25))
    assert(sumSquare(List(1, 2, 3), List(4, 5)) === List(17, 26, 20, 29, 25, 34))
  }

  "Id" can "support monadic and functor ops" in {
    import cats.Id

    val a = Monad[Id].pure(3)
    assert(a === (3: Id[Int]))

    val b = Monad[Id].flatMap(a)(_ + 1)
    assert(b === (4: Id[Int]))

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    val c = for {
      x <- a
      y <- b
    } yield x + y
    assert(c === (7: Id[Int]))
  }

  "IdMonadOps" can "implement pure" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.pure
    assert(pure(123) === 123)
  }

  it can "implement map" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.{pure, map}
    assert(map(pure(123))(_ * 2) === 246)
  }

  it can "implement flatmap" in {
    import scala_cats.chapter04.ex_4_3_1.IdMonadOps.{pure, flatMap}
    assert(flatMap(pure(123))(_ * 2) === 246)
  }

  "either monad" can "make either nicer" in {
    val a: Either[String, Int] = 3.asRight[String]
    val b: Either[String, Int] = 4.asRight[String]

    val res = for {
      x <- a
      y <- b
    } yield x * x + y * y

    assert(res === Right(25))
  }

  def countPositive(nums: List[Int]): Either[String, Int] = {
    nums.foldLeft(0.asRight[String]) {
      case (accumulator, num) => if (num > 0) accumulator.map(_ + 1) else Left("Negative number - abort!")
    }
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

  type ErrorOr[A] = Either[String, A]

  // trait MonadError[F[_], E] extends Monad[F]
  // F is the type of the monad;
  // E is the type of error contained within F.
  import cats.MonadError
  import cats.implicits.{catsSyntaxApplicativeErrorId, catsSyntaxApplicativeId}

  private val monadError = MonadError[ErrorOr, String]

  "monadError" can "represent success" in {

    val success: ErrorOr[Int] = monadError.pure(42)
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
    val failure: ErrorOr[Nothing] = monadError.raiseError("Badness")
    assert(failure == Left("Badness"))

    assert("Badness".raiseError[ErrorOr, Int] == Left("Badness"))
  }

  def handleErrorWith[A](failure: ErrorOr[A], okValue: A): ErrorOr[A] = {
    monadError.handleErrorWith(failure) {
      case "Badness" =>
        monadError.pure(okValue)

      case _ =>
        monadError.raiseError("It's not ok")
    }
  }

  "monadError handle error with" can "handle the error and recover" in {
    val failure: ErrorOr[Nothing] = monadError.raiseError("Badness")
    assert(handleErrorWith(failure, "It's ok") == Right("It's ok"))

    val failure2: ErrorOr[Int] = "Badness".raiseError[ErrorOr, Int]
    assert(handleErrorWith(failure2, 7) == Right(7))
  }

  it can "handle the error and not recover" in {
    val failure: ErrorOr[Nothing] = monadError.raiseError("Oh no")
    assert(handleErrorWith(failure, "It's ok") == Left("It's not ok"))
  }

  it can "handle success" in {
    val failure: ErrorOr[String] = monadError.pure("Success!")
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

  def handleError[A](failure: ErrorOr[A], okValue: A, notOkValue: A): ErrorOr[A] = {
    monadError.handleError(failure) {
      case "Badness" => okValue

      case _ => notOkValue
    }
  }

  "monadError handle error" can "handle the error and recover" in {
    val failure: ErrorOr[Nothing] = monadError.raiseError("Badness")
    assert(handleError[Int](failure, 42, 0) == Right(42))

    val failure2: ErrorOr[Int] = "Badness".raiseError[ErrorOr, Int]
    assert(handleError(failure2, 42, 0) == Right(42))
  }

  it can "handle success" in {
    val failure: ErrorOr[String] = monadError.pure("Success!")
    assert(handleError(failure, "It's ok", "Not ok") == Right("Success!"))
  }

  "monadError ensure" can "return value if predicate satisfied" in {
    assert(monadError.ensure(monadError.pure(42))("Number too low!")(_ < 50) == Right(42))

    assert(42.pure[ErrorOr].ensure("Number too low")(_ < 50) == Right(42))
  }

  it can "return error if predicate not satisfied" in {
    assert(monadError.ensure(monadError.pure(42))("Number too low!")(_ < 40) == Left("Number too low!"))

    assert(42.pure[ErrorOr].ensure("Number too low!")(_ < 40) == Left("Number too low!"))
  }

  val exn: Throwable = new RuntimeException("It's all gone wrong")

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

  //noinspection TypeAnnotation
  object MonadErrors {
    type ExceptionOr[A] = Either[Throwable, A]

    implicit val meTry = MonadError[Try, Throwable]
    implicit val meExceptionOr = MonadError[ExceptionOr, Throwable]
  }

  "validateAge success" can "return a try" in {
    import MonadErrors.meTry
    assert(validateAdult(21) == Success(21))
    assert(validateAdult[Try](21) == Success(21))
  }

  it can "return an either" in {
    import MonadErrors.meExceptionOr
    assert(validateAdult(21) == Right(21))
  }

  "validateAge failure" can "return a try" in {
    import MonadErrors.meTry

    // Following implicit combines with cats.Eq.catsStdEqForTry to provide
    // an eq method for Try[Int]
    implicit val eqT: Eq[Throwable] = Eq.allEqual

    assert(validateAdult(8) === Failure[Int](illegalAgeException(8)))
  }

  it can "return an either" in {
    import MonadErrors.meExceptionOr

    implicit val eqT: Eq[Throwable] = Eq.allEqual

    assert(validateAdult(8) === illegalAgeException(8).asLeft)
  }

  "foldSafeRight" can "sum a long list of numbers safely" in {
    assert(foldRightSafe((1 to 100000).toList, 0L)(_ + _).value == 5000050000L)
    assert(foldRightTextbook((1 to 100000).toList, 0L)(_ + _).value == 5000050000L)
  }

  "writer monad" can "return results" in {
    import cats.implicits.catsSyntaxWriterId
    val value = 123
    val log = Vector("msg1", "msg2", "msg3")

    val wm = 123.writer(log)

    assert(wm.run == ((log, value)))
    assert(wm.written == log)
    assert(wm.value == value)
  }

  private val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield (a + b)

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

  it can "rest the log" in {
    val writer2 = writer1.reset

    assert(writer2.run == ((Vector(), 42)))
  }

  it can "swap log and result over" in {
    val writer2 = writer1.swap

    assert(writer2.run == ((42, Vector("a", "b", "c", "x", "y", "z"))))
  }
}