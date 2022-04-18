package scala_cats.casestudies.cdrt.take3

import scala_cats.UnitSpec
import scala_cats.casestudies.cdrt.take3.GCounterLessRestrictive.mapInstance

class Take3GCounterLessRestrictiveSpec extends UnitSpec {

  "map increment" can "increment existing counts using different types" in {
    import cats.instances.int._ // add ints

    val gCounter = GCounterLessRestrictive[Map]

    gCounter.increment(Map("a" -> 7, "b" -> 3))("a", 1) should equal(
      Map("a" -> 8, "b" -> 3)
    )

    gCounter.increment(Map(1 -> Set("7"), 2 -> Set("3")))(1, Set("1")) should equal(
      Map(1 -> Set("1", "7"), 2 -> Set("3"))
    )
  }

  it can "add new count using different types" in {
    import cats.instances.int._ // add ints

    val gCounter = GCounterLessRestrictive[Map]

    gCounter.increment(Map("m1" -> 0, "m2" -> 5))("m3", 2) should equal(
      Map("m1" -> 0, "m2" -> 5, "m3" -> 2)
    )

    gCounter.increment(Map(1 -> Set("7"), 2 -> Set("3")))(3, Set("1")) should equal(
      Map(1 -> Set("7"), 2 -> Set("3"), 3 -> Set("1"))
    )
  }

  "map merge" can "take the highest int count for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounterLessRestrictive[Map].merge(
      Map("a" -> 7, "b" -> 3)
    )(
      Map("a" -> 2, "b" -> 5)
    ) should equal(
      Map("a" -> 7, "b" -> 5)
    )
  }

  it can "take the merged set counts for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.setInstance // take set merges

    GCounterLessRestrictive[Map].merge(
      Map(1 -> Set("7"), 2 -> Set("3"))
    )(
      Map(1 -> Set("9"), 2 -> Set("2"))
    ) should equal(
      Map(1 -> Set("7", "9"), 2 -> Set("2", "3"))
    )
  }

  "map count" can "calculate the total for different types" in {
    import cats.instances.int._ // add ints

    val gCounter = GCounterLessRestrictive[Map]

    gCounter.total(Map("a" -> 7, "b" -> 5)) should equal(12)

    gCounter.total(Map(1 -> Set("7", "9"), 2 -> Set("2", "3"))) should equal(
      Set("2", "3", "7", "9")
    )
  }

  "my map increment" can "increment existing counts using different types" in {
    import cats.instances.int._ // add ints

    val gCounterMyMap = GCounterLessRestrictive[MyMap]

    gCounterMyMap.increment(MyMap(List("a" -> 7, "b" -> 3)))("a", 1) should equal(
      MyMap(List("a" -> 8, "b" -> 3))
    )

    gCounterMyMap.increment(MyMap(List(1 -> Set("7"), 2 -> Set("3"))))(1, Set("1")) should equal(
      MyMap(List(1 -> Set("1", "7"), 2 -> Set("3")))
    )
  }

  it can "add new count using different types" in {
    import cats.instances.int._ // add ints

    val gCounterMyMap = GCounterLessRestrictive[MyMap]

    gCounterMyMap.increment(MyMap(List("m1" -> 0, "m2" -> 5)))("m3", 2) should equal(
      MyMap(List("m1" -> 0, "m2" -> 5, "m3" -> 2))
    )

    gCounterMyMap.increment(MyMap(List(1 -> Set("7"), 2 -> Set("3"))))(3, Set("1")) should equal(
      MyMap(List(1 -> Set("7"), 2 -> Set("3"), 3 -> Set("1")))
    )
  }

  "my map merge" can "take the highest int count for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounterLessRestrictive[MyMap].merge(
      MyMap(List("a" -> 7, "b" -> 3))
    )(
      MyMap(List("a" -> 2, "b" -> 5))
    ) should equal(
      MyMap(List("a" -> 7, "b" -> 5))
    )
  }

  it can "take the merged set counts for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.setInstance // take set merges

    GCounterLessRestrictive[MyMap].merge(
      MyMap(List(1 -> Set("7"), 2 -> Set("3")))
    )(
      MyMap(List(1 -> Set("9"), 2 -> Set("2")))
    ) should equal(
      MyMap(List(1 -> Set("7", "9"), 2 -> Set("2", "3")))
    )
  }

  "my map count" can "calculate the total for different types" in {
    import cats.instances.int._ // add ints

    val gCounterMyMap = GCounterLessRestrictive[MyMap]

    gCounterMyMap.total(MyMap(List("a" -> 7, "b" -> 5))) should equal(12)

    gCounterMyMap.total(MyMap(List(1 -> Set("7", "9"), 2 -> Set("2", "3")))) should equal(
      Set("2", "3", "7", "9")
    )
  }
}