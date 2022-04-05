package scala_cats.casestudies.cdrt.take3

import scala_cats.UnitSpec

class GCounterSpec extends UnitSpec {

  "increment" can "increment existing integer count" in { // add ints
    import KeyValueStore.mapInstance
    import cats.instances.map._

    val gCounter = GCounter[Map, String, Int]

    val expectedMap = Map("a" -> 8, "b" -> 3)

    gCounter.increment(Map("a" -> 7, "b" -> 3))("a", 1) should equal(expectedMap)
  }

//  it can "add new integer count" in { // add ints
//
//    val gCounter = GCounter[Map, String, Int]
//
//    val expectedMap = Map("m1" -> 0, "m2" -> 5, "m3" -> 2)
//
//    gCounter.increment(Map("m1" -> 0, "m2" -> 5))("m3", 2) should equal(expectedMap)
//  }
//
//  "merge" can "take the highest int count for each machine" in { // take highest int in pair
//
//    val gCounter = GCounter[Map, String, Int]
//
//    val expectedMap = Map("a" -> 7, "b" -> 5)
//
//    gCounter.merge(Map("a" -> 7, "b" -> 3))(Map("a" -> 2, "b" -> 5)) should equal(expectedMap)
//  }
//
//  "count" can "calculate the int total" in { // add ints
//
//    val gCounter = GCounter[Map, String, Int]
//
//    gCounter.total(Map("a" -> 7, "b" -> 5)) should equal(12)
//  }
}