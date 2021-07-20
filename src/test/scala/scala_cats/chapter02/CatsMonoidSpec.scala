package scala_cats.chapter02

import scala_cats.UnitSpec

//noinspection OptionEqualsSome
class CatsMonoidSpec extends UnitSpec {

  "monoid" can "support strings" in {
    import cats.Monoid
    import cats.instances.string._

    assert(Monoid[String].combine("Hi ", "there") === "Hi there")
    assert(Monoid.apply[String].combine("Hi ", "there") === "Hi there")
    assert(Monoid[String].empty === "")
    assert(Monoid.apply[String].empty === "")
  }

  it can "support ints" in {
    import cats.Monoid
    import cats.instances.int._

    assert(Monoid[Int].combine(5, 6) === 11)
    assert(Monoid[Int].empty === 0)
  }

  it can "support options" in {
    import cats.Monoid
    import cats.instances.int._
    import cats.instances.option._

    assert(Monoid[Option[Int]].combine(Some(25), Some(6)) === Some(31))
    assert(Monoid[Option[Int]].combine(Some(25), None) === Some(25))
    assert(Monoid[Option[Int]].combine(Some(0), None) === Some(0))
    assert(Monoid[Option[Int]].combine(None, Some(0)) === Some(0))
    assert(Monoid[Option[Int]].combine(None, None).isEmpty)
    assert(Monoid[Option[Int]].empty.isEmpty)
  }

  it can "support syntax" in {
    import cats.Monoid
    import cats.instances.string._
    import cats.syntax.semigroup._ // for |+|

    assert(("Hi " |+| "there") === "Hi there")
    assert(("Hi " |+| "there" |+| Monoid[String].empty) === "Hi there")

    import cats.instances.int._
    assert((1 |+| 2 |+| Monoid[Int].empty) === 3)
  }
}