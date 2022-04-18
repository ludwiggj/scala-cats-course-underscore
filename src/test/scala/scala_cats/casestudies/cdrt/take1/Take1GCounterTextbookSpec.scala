package scala_cats.casestudies.cdrt.take1

import scala_cats.UnitSpec

class Take1GCounterTextbookSpec extends UnitSpec {

  "increment" can "increment existing count" in {
    GCounterTextbook(Map("m1" -> 1, "m2" -> 5)).increment("m1", 7) should equal(
      GCounterTextbook(Map("m1" -> 8, "m2" -> 5))
    )
  }

  it can "add new count" in {
    GCounterTextbook(Map("m1" -> 0, "m2" -> 5)).increment("m3", 2) should equal(
      GCounterTextbook(Map("m1" -> 0, "m2" -> 5, "m3" -> 2))
    )
  }

  "merge" can "take the highest count for each machine" in {
    GCounterTextbook(Map("m1" -> 0, "m2" -> 5)).merge(
      GCounterTextbook(Map("m1" -> 3, "m2" -> 0))
    ) should equal(
      GCounterTextbook(Map("m1" -> 3, "m2" -> 5))
    )
  }

  it can "calculate highest counts when new count on LHS" in {
    GCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2)).merge(
      GCounterTextbook(Map("m1" -> 3, "m2" -> 6))
    ) should equal(
      GCounterTextbook(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))
    )
  }

  it can "calculate highest counts when new count on RHS" in {
    GCounterTextbook(Map("m1" -> 3, "m2" -> 6)).merge(
      GCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2))
    ) should equal(
      GCounterTextbook(Map("m1" -> 4, "m2" -> 6, "m3" -> 2))
    )
  }

  "count" can "calculate the total" in {
    GCounterTextbook(Map("m1" -> 4, "m2" -> 5, "m3" -> 2)).total should equal(11)
  }
}