package scala_cats.casestudies.cdrt.take1

import scala_cats.UnitSpec

class GenericGCounterSpec extends UnitSpec {

  "increment" can "increment existing integer count" in {
    // Default implicit is: Semigroup, implicit def catsKernelCommutativeGroupForInt
    // import BoundedSemiLattice.intInstance is incorrect for this method (want to add numbers not take the max)
    import cats.instances.int.catsKernelStdGroupForInt

    val gCounter = GenericGCounter(Map("m1" -> 1, "m2" -> 5))

    val expectedCounter = GenericGCounter(Map("m1" -> 8, "m2" -> 5))

    gCounter.increment("m1", 7) should equal(expectedCounter)
  }

  it can "add new integer count" in {
    import cats.instances.int.catsKernelStdGroupForInt

    val gCounter = GenericGCounter(Map("m1" -> 0, "m2" -> 5))

    val expectedCounter = GenericGCounter(Map("m1" -> 0, "m2" -> 5, "m3" -> 2))

    gCounter.increment("m3", 2) should equal(expectedCounter)
  }

  "increment" can "increment existing set" in {
    // Semigroup implicit def catsKernelBoundedSemilatticeForSet[A]
    import BoundedSemiLattice.setInstance

    val gCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5")))

    val expectedCounter = GenericGCounter(Map("m1" -> Set("1", "7"), "m2" -> Set("5")))

    gCounter.increment("m1", Set("1", "7")) should equal(expectedCounter)
  }

  it can "add new set" in {
    // Semigroup implicit def catsKernelBoundedSemilatticeForSet[A]
    import BoundedSemiLattice.setInstance

    val gCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5")))

    val expectedCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("2")))

    gCounter.increment("m3", Set("2")) should equal(expectedCounter)
  }

  "merge" can "take the highest int count for each machine" in {
    import BoundedSemiLattice.intInstance

    val gCounter = GenericGCounter(Map("m1" -> 1, "m2" -> 5))
    val anotherCounter = GenericGCounter(Map("m1" -> 3, "m2" -> 0))

    val expectedCounter = GenericGCounter(Map("m1" -> 3, "m2" -> 5))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "calculate highest int counts when new count on LHS" in {
    import BoundedSemiLattice.intInstance

    val gCounter = GenericGCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))
    val anotherCounter = GenericGCounter(Map("m1" -> 3, "m2" -> 6))

    val expectedCounter = GenericGCounter(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "calculate highest counts when new count on RHS" in {
    import BoundedSemiLattice.intInstance

    val gCounter = GenericGCounter(Map("m1" -> 3, "m2" -> 6))
    val anotherCounter = GenericGCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))

    val expectedCounter = GenericGCounter(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "merge the sets for each machine" in {
    import BoundedSemiLattice.setInstance

    val gCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5")))

    val anotherCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3")))

    val expectedCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5")))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "merge the sets for each machine when new set on LHS" in {
    import BoundedSemiLattice.setInstance

    val gCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("12", "15")))

    val anotherCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3")))

    val expectedCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5"), "m3" -> Set("12", "15")))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "merge the sets for each machine when new set on RHS" in {
    import BoundedSemiLattice.setInstance

    val gCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("12", "15")))

    val anotherCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3"), "m3" -> Set("12", "15")))

    val expectedCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("2", "3", "5"), "m3" -> Set("12", "15")))

    gCounter.merge(anotherCounter) should equal(expectedCounter)
    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  "count" can "calculate the int total" in {
    // import BoundedSemiLattice.intInstance is incorrect for this method (want to add numbers not take the max)
    import cats.instances.int.catsKernelStdGroupForInt

    val aCounter = GenericGCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))

    aCounter.total should equal(11)
    aCounter.totalTextbook should equal(11)
  }

  it can "combine the sets into a total" in {
    import BoundedSemiLattice.setInstance

    val aCounter = GenericGCounter(Map("m1" -> Set("1"), "m2" -> Set("5"), "m3" -> Set("12", "15", "5")))

    aCounter.total should equal(Set("1", "5", "12", "15"))
    aCounter.totalTextbook should equal(Set("1", "5", "12", "15"))
  }
}