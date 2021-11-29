package scala_cats.chapter06

import cats.Semigroupal
import scala_cats.UnitSpec
import scala_cats.chapter06.SemigroupalWorkout.{Cat, Dog, FancyApply, add3Numbers, anotherShowDog, showDog}

class SemigroupalSpec extends UnitSpec {
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

  "tuple3" can "combine 3 options" in {
    assert(Semigroupal.tuple3(Option(1), Option(2), Option(3)).contains((1, 2, 3)))
    assert(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]).isEmpty)
  }

  "tupled" can "combine 3 options" in {
    import cats.syntax.apply._
    assert((Option(1), Option(2), Option(3)).tupled.contains((1, 2, 3)))
    assert((Option(1), Option(2), Option.empty[Int]).tupled.isEmpty)
  }

  "map3" can "combine 3 options" in {
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
    assert((Option(garfield.name), Option(garfield.born), Option(garfield.colour)).mapN(Cat.apply).contains(garfield))
  }

  it can "fail to construct a cat if name is missing" in {
    import cats.syntax.apply._
    val garfield = Cat("Garfield", 1978, "Orange & black")
    assert((None, Option(garfield.born), Option(garfield.colour)).mapN(Cat.apply).isEmpty)
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
}