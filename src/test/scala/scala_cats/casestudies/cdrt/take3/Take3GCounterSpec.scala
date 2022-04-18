package scala_cats.casestudies.cdrt.take3

import scala_cats.UnitSpec

import GCounter.mapInstance

class Take3GCounterSpec extends UnitSpec {

  "map increment" can "increment existing integer count" in {
    import cats.instances.int._ // add ints

    GCounter[Map, String, Int].increment(Map("a" -> 7, "b" -> 3))("a", 1) should equal(
      Map("a" -> 8, "b" -> 3)
    )
  }

  it can "add new integer count" in {
    import cats.instances.int._ // add ints

    GCounter[Map, String, Int].increment(Map("m1" -> 0, "m2" -> 5))("m3", 2) should equal(
      Map("m1" -> 0, "m2" -> 5, "m3" -> 2)
    )
  }

  "map merge" can "take the highest int count for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounter[Map, String, Int].merge(
      Map("a" -> 7, "b" -> 3)
    )(
      Map("a" -> 2, "b" -> 5)
    ) should equal(
      Map("a" -> 7, "b" -> 5)
    )
  }

  "map count" can "calculate the int total" in {
    import cats.instances.int._ // add ints

    GCounter[Map, String, Int].total(Map("a" -> 7, "b" -> 5)) should equal(12)
  }

  "my map increment" can "increment existing integer count" in {
    import cats.instances.int._ // add ints

    GCounter[MyMap, String, Int].increment(MyMap(List("a" -> 7, "b" -> 3)))("a", 1) should equal(
      MyMap(List("a" -> 8, "b" -> 3))
    )
  }

  it can "add new integer count" in {
    import cats.instances.int._ // add ints

    GCounter[MyMap, String, Int].increment(MyMap(List("m1" -> 0, "m2" -> 5)))("m3", 2) should equal(
      MyMap(List("m1" -> 0, "m2" -> 5, "m3" -> 2))
    )
  }

  "my map merge" can "take the highest int count for each machine" in {
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounter[MyMap, String, Int].merge(
      MyMap(List("a" -> 7, "b" -> 3))
    )(
      MyMap(List("a" -> 2, "b" -> 5))
    ) should equal(
      MyMap(List("a" -> 7, "b" -> 5))
    )
  }

  "my map count" can "calculate the int total" in {
    import cats.instances.int._ // add ints

    GCounter[MyMap, String, Int].total(MyMap(List("a" -> 7, "b" -> 5))) should equal(12)
  }
}