package scala_cats.chapter06

import cats.Semigroupal
import scala_cats.UnitSpec
import scala_cats.chapter06.ParallelWorkout.optionToList
import scala_cats.chapter06.SemigroupalWorkout.FancyApply.FancyCat
import scala_cats.chapter06.SemigroupalWorkout.Futures.futurePair
import scala_cats.chapter06.SemigroupalWorkout.{Cat, Dog, FancyApply, add3Numbers, anotherShowDog, showDog}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class SemigroupalAndParallelSpec extends UnitSpec {
  type ErrorOr[A] = Either[Vector[String], A]

  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)

  type ErrorOrList[A] = Either[List[String], A]

  val errorL1: ErrorOrList[Int] = Left(List("Error 1"))
  val errorL2: ErrorOrList[Int] = Left(List("Error 2"))

  val add = (x: Int, y: Int) => x + y

  "parser" can "add 3 parsed numbers" in {
    assert(add3Numbers("1", "2", "3") == Right(6))
  }

  it can "return a single failure for single parse failure" in {
    assert(add3Numbers("1", "two", "3") == Left("Couldn't read two"))
  }

  it can "return first failure only for multiple parse failures" in {
    assert(add3Numbers("one", "two", "3") == Left("Couldn't read one"))
  }

  "semigroupal" can "calculate product of two options when both inputs are a some" in {
    assert(Semigroupal[Option].product(Some(123), Some("abc")).contains((123, "abc")))
  }

  it can "calculate product of two options when either input is none" in {
    assert(Semigroupal[Option].product(None, Some("abc")).isEmpty)
    assert(Semigroupal[Option].product(Some(123), None).isEmpty)
  }

  it can "combine futures" in {
    assert(Await.result(futurePair, 1.second) == (("Hello", 123)))
  }

  it can "combine lists" in {
    assert(Semigroupal[List].product(List(1, 2), List(3, 4)) == List((1, 3), (1, 4), (2, 3), (2, 4)))
  }

  it can "combine eithers, retaining first error only" in {
    assert(Semigroupal[ErrorOr].product(error1, error2) == error1)
    assert(Semigroupal[ErrorOrList].product(errorL1, errorL2) == errorL1)

    import cats.syntax.apply._ // for tupled

    assert((error1, error2).tupled == error1)
    assert((errorL1, errorL2).tupled == errorL1)
  }

  "tuple3" can "combine 3 options" in {
    assert(Semigroupal.tuple3(Option(1), Option(2), Option(3)).contains((1, 2, 3)))
    assert(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]).isEmpty)
  }

  "tupled" can "combine 3 options" in {
    import cats.syntax.apply._ // for tupled, for tuples up to 22 values
    assert((Option(1), Option(2), Option(3)).tupled.contains((1, 2, 3)))
    assert((Option(1), Option(2), Option.empty[Int]).tupled.isEmpty)
  }

  "map3" can "combine 3 options" in { // supports up to 22 parameters
    // def map3[F[_], A0, A1, A2, Z](f0:F[A0], f1:F[A1], f2:F[A2])(f: (A0, A1, A2) => Z)(implicit semigroupal: Semigroupal[F], functor: Functor[F]): F[Z] =
    //    functor.map(semigroupal.product(f0, semigroupal.product(f1, f2))) { case (a0, (a1, a2)) => f(a0, a1, a2) }

    assert(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _).contains(6))
    assert(Semigroupal.map3(Option(1), Option(2), Option.empty[Int])(_ + _ + _).isEmpty)
  }

  "contramap2" can "show a dog" in {
    assert(showDog.show(Dog("Bonzo", 5)) == "Bonzo 5")
  }

  "imap2" can "show a dog" in {
    assert(anotherShowDog.show(Dog("Bonzo", 5)) == "Bonzo 5")
  }

  "mapN" can "construct a cat" in {
    import cats.syntax.apply._
    val garfield = Cat("Garfield", 1978, "Orange & black")

    // def mapN[Z](f: (A0, A1, A2) => Z)(implicit functor: Functor[F], semigroupal: Semigroupal[F]): F[Z] =
    //   Semigroupal.map3(t3._1, t3._2, t3._3)(f)

    assert((Option(garfield.name), Option(garfield.born), Option(garfield.colour)).mapN(Cat.apply).contains(garfield))
  }

  it can "fail to construct a cat if name is missing" in {
    import cats.syntax.apply._
    val garfield = Cat("Garfield", 1978, "Orange & black")
    assert((None, Option(garfield.born), Option(garfield.colour)).mapN(Cat.apply).isEmpty)
  }

  it can "construct a cat in the future" in {
    import cats.syntax.apply._
    import java.util.concurrent.Executors
    import scala.concurrent.{ExecutionContext, Future} // For mapN

    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    val futureCat = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
      ).mapN(FancyCat.apply)

    assert(Await.result(futureCat, 1.second) == FancyCat("Garfield", 1978, List("Lasagne")))
  }

  it can "add two numbers" in {
    import cats.syntax.apply._ // for mapN
    assert((success1, success2).mapN(add) == Right(3))
  }

  it can "fail to add two numbers, retaining first error only" in {
    import cats.syntax.apply._ // for mapN
    assert((error1, error2).mapN(add) == error1)
    assert((errorL1, errorL2).mapN(add) == errorL1)
  }

  "tupleToFancyCat" can "construct a cat from a tuple" in {
    import FancyApply._
    val garfield = FancyCat("Garfield", 1978, List("Lasagne", "Scotch on the Rocks"))
    assert(tupleToFancyCat(garfield.name, garfield.yearOfBirth, garfield.favouriteFoods) == garfield)
  }

  "fancyCatToTuple" can "construct a tuple from a cat" in {
    import FancyApply._
    val garfield = FancyCat("Garfield", 1978, List("Lasagne", "Scotch on the Rocks"))
    assert(fancyCatToTuple(garfield) == ((garfield.name, garfield.yearOfBirth, garfield.favouriteFoods)))
  }

  "catMonoid" can "combine cats" in {
    import FancyApply.catMonoid
    import cats.syntax.semigroup._
    val garfield = FancyCat("Garfield", 1978, List("Lasagne"))
    val heathcliff = FancyCat("Heathcliff", 1988, List("Junk Food"))
    assert((garfield |+| heathcliff) == FancyCat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food")))
  }

  "parTupled" can "retrieve multiple failures" in {
    import cats.syntax.parallel._ // for parTupled

    assert((error1, error2).parTupled == Left(Vector("Error 1", "Error 2")))

    assert((errorL1, errorL2).parTupled == Left(List("Error 1", "Error 2")))
  }

  "parMapN" can "add two numbers" in {
    import cats.syntax.parallel._ // for parMapN
    assert((success1, success2).parMapN(add) == Right(3))
  }

  it can "fail to add two numbers, retaining all errors" in {
    import cats.syntax.parallel._ // for parMapN
    assert((error1, error2).parMapN(add) == Left(Vector("Error 1", "Error 2")))
    assert((errorL1, errorL2).parMapN(add) == Left(List("Error 1", "Error 2")))
  }

  "optionToList" can "convert option to list" in {
    assert(optionToList(Some(1)) == List(1))
    assert(optionToList(None) == List())
  }

  // NOTE: Turn on implicit hints to see where the parallel support for list comes from
  "parallel list" can "zip lists" in {
    import cats.instances.list._
    import cats.syntax.parallel._

    assert((List(1, 2), List(3, 4)).parTupled == List((1, 3), (2, 4)))
  }
}