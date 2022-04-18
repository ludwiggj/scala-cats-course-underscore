package scala_cats.casestudies.cdrt.take2

import scala_cats.UnitSpec

class Take2GenericGCounterTextbookSpec extends UnitSpec {

  "increment" can "increment existing integer count" in {
    // Default implicit is: Semigroup, implicit def catsKernelCommutativeGroupForInt
    // import BoundedSemiLattice.intInstance is incorrect for this method (want to add numbers not take the max)
    import cats.instances.int.catsKernelStdGroupForInt

    GenericGCounterTextbook(Map("m1" -> 1, "m2" -> 5)).increment("m1", 7) should equal(
      GenericGCounterTextbook(Map("m1" -> 8, "m2" -> 5))
    )
  }

  it can "add new integer count" in {
    import cats.instances.int.catsKernelStdGroupForInt

    GenericGCounterTextbook(Map("m1" -> 0, "m2" -> 5)).increment("m3", 2) should equal(
      GenericGCounterTextbook(Map("m1" -> 0, "m2" -> 5, "m3" -> 2))
    )
  }

  "increment" can "increment existing set" in {
    // Semigroup implicit def catsKernelBoundedSemilatticeForSet[A]
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5")))
      .increment("m1", Set("1", "7")) should equal(
      GenericGCounterTextbook(Map("m1" -> Set("1", "7"), "m2" -> Set("5")))
    )
  }

  it can "add new set" in {
    // Semigroup implicit def catsKernelBoundedSemilatticeForSet[A]
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"))).increment("m3", Set("2")) should equal(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("2")))
    )
  }

  "merge" can "take the highest int count for each machine" in {
    import BoundedSemiLattice.intInstance // Takes the max for each int

    GenericGCounterTextbook(Map("m1" -> 1, "m2" -> 5)).merge(
      GenericGCounterTextbook(Map("m1" -> 3, "m2" -> 0))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> 3, "m2" -> 5))
    )
  }

  it can "calculate highest int counts when new count on LHS" in {
    import BoundedSemiLattice.intInstance

    GenericGCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2)).merge(
      GenericGCounterTextbook(Map("m1" -> 3, "m2" -> 6))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))
    )
  }

  it can "calculate highest counts when new count on RHS" in {
    import BoundedSemiLattice.intInstance

    GenericGCounterTextbook(Map("m1" -> 3, "m2" -> 6)).merge(
      GenericGCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))
    )
  }

  it can "merge the sets for each machine" in {
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"))).merge(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3")))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5")))
    )
  }

  it can "merge the sets for each machine when new set on LHS" in {
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("12", "15"))).merge(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3")))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5"), "m3" -> Set("12", "15")))
    )
  }

  it can "merge the sets for each machine when new set on RHS" in {
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"))).merge(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3"), "m3" -> Set("12", "15")))
    ) should equal(
      GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5"), "m3" -> Set("12", "15")))
    )
  }

  "count" can "calculate the int total" in {
    // import BoundedSemiLattice.intInstance is incorrect for this method (want to add numbers not take the max)
    import cats.instances.int.catsKernelStdGroupForInt

    GenericGCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2)).total should equal(11)
  }

  it can "combine the sets into a total" in {
    import BoundedSemiLattice.setInstance

    GenericGCounterTextbook(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("12", "15", "5"))).total should equal(
      Set("1", "5", "12", "15")
    )
  }
}