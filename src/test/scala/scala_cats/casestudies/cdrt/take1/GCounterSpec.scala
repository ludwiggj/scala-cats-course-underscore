package scala_cats.casestudies.cdrt.take1

import scala_cats.UnitSpec

class GCounterSpec extends UnitSpec {

  "increment" can "increment existing count" in {
    val gCounter = GCounter(Map("m1" -> 1, "m2" -> 5))

    val expectedCounter = GCounter(Map("m1" -> 8, "m2" -> 5))

    gCounter.increment("m1", 7) should equal(expectedCounter)
  }

  it can "add new count" in {
    val gCounter = GCounter(Map("m1" -> 0, "m2" -> 5))

    val expectedCounter = GCounter(Map("m1" -> 0, "m2" -> 5, "m3" -> 2))

    gCounter.increment("m3", 2) should equal(expectedCounter)
  }

  "merge" can "take the highest count for each machine" in {
    val gCounter = GCounter(Map("m1" -> 0, "m2" -> 5))
    val anotherCounter = GCounter(Map("m1" -> 3, "m2" -> 0))

    val expectedCounter = GCounter(Map("m1" -> 3, "m2" -> 5))

    gCounter.merge(anotherCounter) should equal(expectedCounter)

    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "calculate highest counts when new count on LHS" in {
    val gCounter = GCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))
    val anotherCounter = GCounter(Map("m1" -> 3, "m2" -> 6))

    val expectedCounter = GCounter(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))

    gCounter.merge(anotherCounter) should equal(expectedCounter)

    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  it can "calculate highest counts when new count on RHS" in {
    val gCounter = GCounter(Map("m1" -> 3, "m2" -> 6))
    val anotherCounter = GCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))

    val expectedCounter = GCounter(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))

    gCounter.merge(anotherCounter) should equal(expectedCounter)

    gCounter.mergeTextbook(anotherCounter) should equal(expectedCounter)
  }

  "count" can "calculate the total" in {
    val aCounter = GCounter(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))

    aCounter.total should equal(11)

    aCounter.totalTextbook should equal(11)
  }
}