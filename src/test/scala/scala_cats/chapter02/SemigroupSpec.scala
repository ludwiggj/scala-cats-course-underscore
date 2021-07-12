package scala_cats.chapter02

import scala_cats.UnitSpec
import scala_cats.chapter02.Semigroup.associativeLaw

class SemigroupSpec extends UnitSpec {

  "boolean and" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanAndMonoid
    assert(associativeLaw(false, false, false))
    assert(associativeLaw(false, false, true))
    assert(associativeLaw(false, true, false))
    assert(associativeLaw(false, true, true))
    assert(associativeLaw(true, false, false))
    assert(associativeLaw(true, false, true))
    assert(associativeLaw(true, true, false))
    assert(associativeLaw(true, true, true))
  }

  "boolean or" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanOrMonoid
    assert(associativeLaw(false, false, false))
    assert(associativeLaw(false, false, true))
    assert(associativeLaw(false, true, false))
    assert(associativeLaw(false, true, true))
    assert(associativeLaw(true, false, false))
    assert(associativeLaw(true, false, true))
    assert(associativeLaw(true, true, false))
    assert(associativeLaw(true, true, true))
  }

  "boolean Xor" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanXorMonoid
    assert(associativeLaw(false, false, false))
    assert(associativeLaw(false, false, true))
    assert(associativeLaw(false, true, false))
    assert(associativeLaw(false, true, true))
    assert(associativeLaw(true, false, false))
    assert(associativeLaw(true, false, true))
    assert(associativeLaw(true, true, false))
    assert(associativeLaw(true, true, true))
  }

  "boolean Xnor" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_3.BooleanMonoid.BooleanOps.booleanXnorMonoid
    assert(associativeLaw(false, false, false))
    assert(associativeLaw(false, false, true))
    assert(associativeLaw(false, true, false))
    assert(associativeLaw(false, true, true))
    assert(associativeLaw(true, false, false))
    assert(associativeLaw(true, false, true))
    assert(associativeLaw(true, true, false))
    assert(associativeLaw(true, true, true))
  }

  "set add" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetMonoid.setAddMonoid
    assert(associativeLaw(Set(1), Set(2), Set(3)))
  }

  it can "add two sets together" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetSemigroup.setAddSemigroup
    assert(Semigroup[Set[Int]].combine(Set(1), Set(2)) == Set(1, 2))
    assert(Semigroup[Set[String]].combine(Set("A"), Set("B", "C")) == Set("A", "B", "C"))
  }

  "set union" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetSemigroup.setUnionSemigroup
    assert(associativeLaw(Set(1), Set(2), Set(3)))
  }

  "set intersection" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetSemigroup.setIntersectSemigroup
    assert(associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)))
  }

  "set difference" can "not obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetSemigroup.setDifferenceNotASemigroup
    assert(associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)))
  }

  "set symmetric difference" can "obey associative law" in {
    import scala_cats.chapter02.exercises.ex_2_4.SetSemigroup.setSymmetricDifferenceSemigroup
    assert(associativeLaw(Set(1, 2), Set(2, 3), Set(3, 4)))
  }
}