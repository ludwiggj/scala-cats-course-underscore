package scala_cats.chapter02

import scala_cats.UnitSpec

class CatsSemigroupSpec extends UnitSpec {

  "semigroup" can "combine strings" in {
    import cats.Semigroup
    import cats.instances.string._

    assert(Semigroup[String].combine("Hi ", "there") === "Hi there")
    assert(Semigroup.apply[String].combine("Hi ", "there") === "Hi there")
  }
}