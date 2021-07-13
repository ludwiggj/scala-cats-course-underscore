package scala_cats.chapter02

import scala_cats.UnitSpec
import scala_cats.chapter02.Monoid.identityLaw

class MonoidSpec extends UnitSpec {

  "boolean and" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanAndMonoid
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  "boolean or" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanOrMonoid
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  "boolean Xor" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanXorMonoid
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  "boolean Xnor" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanXnorMonoid
    assert(identityLaw(true))
    assert(identityLaw(false))
  }

  "set add" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetMonoid.setAddMonoid
    assert(identityLaw(Set(1, 2, 3)))
    assert(identityLaw(Set.empty[Int]))
  }

  "set union" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetMonoid.setUnionMonoid
    assert(identityLaw(Set(1, 2, 3)))
    assert(identityLaw(Set.empty[Int]))
  }

  "set symmetric difference" can "obey identity law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetMonoid.setSymmetricDifferenceMonoid
    assert(identityLaw(Set(1, 2, 3)))
    assert(identityLaw(Set.empty[Int]))
  }
}