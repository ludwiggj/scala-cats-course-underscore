package scala_cats.chapter01

import scala_cats.UnitSpec
import cats.Eq
import cats.instances.int._
import scala_cats.chapter01.exercises.ex_1_5_5.Cat

class EqSpec extends UnitSpec {
  val eqInt = Eq[Int]

  "eqInt" can "compare ints" in {
    assert(eqInt.eqv(1, 1))
    assert(! eqInt.eqv(1, 0))
  }

  it can "compare ints using symbols" in {
    import cats.syntax.eq._

    // Clashes with scalatest ===, see CatsWorkout instead
    // assert(25 === 25)
    assert(123 =!= 23)
  }

  val cat1: Cat = Cat("Garfield", 38, "orange and black")
  val cat2: Cat = Cat("Heathcliff", 33, "orange and black")

  import Cat.eqCat

  "eqCat" can "compare equal cats" in {
    assert(eqCat.eqv(cat1, cat1))
    assert(eqCat.eqv(cat2, cat2))
  }

  it can "compare unequal cats" in {
    import cats.syntax.eq._

    assert(cat1 =!= cat2)
  }

  import cats.instances.option._

  val optionCat1: Option[Cat] = Option(cat1)
  val optionCat2: Option[Cat] = Option.empty

  it can "compare equal option cats" in {
    import cats.syntax.eq._

    assert(! (optionCat1 =!= optionCat1))
    assert(! (optionCat2 =!= optionCat2))
  }

  it can "compare unequal option cats" in {
    import cats.syntax.eq._

    assert(optionCat1 =!= optionCat2)
  }
}