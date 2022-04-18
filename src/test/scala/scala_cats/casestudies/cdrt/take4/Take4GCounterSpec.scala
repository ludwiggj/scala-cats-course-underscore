package scala_cats.casestudies.cdrt.take4

import scala_cats.UnitSpec
import scala_cats.casestudies.cdrt.take3.MyMap

class Take4GCounterSpec extends UnitSpec {

  "map increment" can "increment existing integer count" in { // add ints
    import KeyValueStore.mapInstance

    GCounter[Map, String, Int].increment(Map("a" -> 7, "b" -> 3))("a", 1) should equal(
      Map("a" -> 8, "b" -> 3)
    )
  }

  it can "add new integer count" in { // add ints
    import KeyValueStore.mapInstance

    GCounter[Map, String, Int].increment(Map("m1" -> 0, "m2" -> 5))("m3", 2) should equal(
      Map("m1" -> 0, "m2" -> 5, "m3" -> 2)
    )
  }

  "map merge" can "take the highest int count for each machine" in {
    import KeyValueStore.mapInstance
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounter[Map, String, Int].merge(
      Map("a" -> 7, "b" -> 3)
    )(
      Map("a" -> 2, "b" -> 5)
    ) should equal(
      Map("a" -> 7, "b" -> 5)
    )
  }

  "map count" can "calculate the int total" in { // add ints
    import KeyValueStore.mapInstance

    GCounter[Map, String, Int].total(Map("a" -> 7, "b" -> 5)) should equal(12)
  }

  "my map increment" can "increment existing integer count" in { // add ints
    import KeyValueStore.myMapInstance

    GCounter[MyMap, String, Int].increment(MyMap(List[(String, Int)]("a" -> 7, "b" -> 3)))("a", 1) should equal(
      MyMap(List[(String, Int)]("a" -> 8, "b" -> 3))
    )
  }

  it can "add new integer count" in { // add ints
    import KeyValueStore.myMapInstance

    GCounter[MyMap, String, Int]
      .increment(MyMap(List[(String, Int)]("m1" -> 0, "m2" -> 5)))("m3", 2) should equal(
      MyMap(List[(String, Int)]("m1" -> 0, "m2" -> 5, "m3" -> 2))
    )
  }

  "my map merge" can "take the highest int count for each machine" in {
    import KeyValueStore.myMapInstance
    import scala_cats.casestudies.cdrt.take2.BoundedSemiLattice.intInstance // take highest int in pair

    GCounter[MyMap, String, Int].merge(
      MyMap(List[(String, Int)]("a" -> 7, "b" -> 3))
    )(
      MyMap(List[(String, Int)]("a" -> 2, "b" -> 5))
    ) should equal(
      MyMap(List[(String, Int)]("a" -> 7, "b" -> 5))
    )
  }

  "my map count" can "calculate the int total" in { // add ints
    import KeyValueStore.myMapInstance

    GCounter[MyMap, String, Int].total(MyMap(List[(String, Int)]("a" -> 7, "b" -> 5))) should equal(12)
  }
}