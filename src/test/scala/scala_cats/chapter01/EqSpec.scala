package scala_cats.chapter01

import scala_cats.UnitSpec
import cats.Eq
import cats.instances.int._
import cats.instances.option._
import cats.tests.StrictCatsEquality
import scala_cats.chapter01.exercises.ex_1_5_5.Cat

// See https://stackoverflow.com/questions/47501997/cats-eqs-with-scalatest
class EqSpec extends UnitSpec with StrictCatsEquality {
  val eqInt = Eq[Int]

  val cat1: Cat = Cat("Garfield", 38, "orange and black")
  val cat2: Cat = Cat("Heathcliff", 33, "orange and black")

  import Cat.eqCat

  val optionCat1: Option[Cat] = Option(cat1)
  val optionCat2: Option[Cat] = Option.empty

  "eqInt" can "compare ints" in {
    assert(eqInt.eqv(1, 1))
    assert(! eqInt.eqv(1, 0))
  }

  it can "compare cats" in {
    assert(eqCat.eqv(cat1, cat1))
    assert(eqCat.eqv(cat2, cat2))
    assert(! eqCat.eqv(cat1, cat2))
  }

  it can "compare option cats" in {
    val eqOptionCat = Eq[Option[Cat]]

    assert(eqOptionCat.eqv(optionCat1, optionCat1))
    assert(eqOptionCat.eqv(optionCat2, optionCat2))
    assert(! eqOptionCat.eqv(optionCat1, optionCat2))
  }

  "===" can "compare ints" in {
    assert(25 === 25)
    assert(!(25 === 251))
  }

  it can "compare cats" in {
    assert(cat1 === cat1)
    assert(cat2 === cat2)
    assert(! (cat1 === cat2))
  }

  it can "compare equal option cats" in {
    assert(optionCat1 === optionCat1)
    assert(optionCat2 === optionCat2)
    assert(! (optionCat1 === optionCat2))
  }

  "=!=" can "compare ints" in {
    import cats.syntax.eq._
    assert(123 =!= 23)
    assert(! (123 =!= 123))
  }

  it can "compare cats" in {
    import cats.syntax.eq._
    assert(cat1 =!= cat2)
    assert(! (cat1 =!= cat1))
  }

  it can "compare option cats" in {
    import cats.syntax.eq._

    assert(optionCat1 =!= optionCat2)
    assert(! (optionCat1 =!= optionCat1))
    assert(! (optionCat2 =!= optionCat2))
  }

  "!==" can "compare ints" in {
    assert(123 !== 23)
    assert(! (123 !== 123))
  }

  it can "compare cats" in {
    assert(cat1 !== cat2)
    assert(! (cat1 !== cat1))
  }

  it can "compare option cats" in {
    assert(optionCat1 !== optionCat2)
    assert(! (optionCat1 !== optionCat1))
    assert(! (optionCat2 !== optionCat2))
  }
}