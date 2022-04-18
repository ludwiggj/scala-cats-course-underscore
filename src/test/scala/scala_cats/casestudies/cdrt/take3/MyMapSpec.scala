package scala_cats.casestudies.cdrt.take3

import scala_cats.UnitSpec

class MyMapSpec extends UnitSpec {

  "put" can "add new value" in {
    MyMap(List(1 -> "one")).put(2, "two") should equal(
      MyMap(List(1 -> "one", 2 -> "two"))
    )
  }

  it can "update existing value" in {
    MyMap(List(1 -> "one", 2 -> "two")).put(2, "too") should equal(
      MyMap(List(1 -> "one", 2 -> "too"))
    )
  }

  "get" can "return value if present" in {
    MyMap(List(1 -> "one", 2 -> "two")).get(2) should equal(Option("two"))
  }

  it can "return None if not present" in {
    MyMap(List(1 -> "one", 2 -> "two")).get(3) should equal(Option.empty[String])
  }

  "getWithIndex" can "return value with index if present" in {
    MyMap(List(1 -> "one", 2 -> "two")).getWithIndex(2) should equal(Option(("two", 1)))
  }

  it can "return None if not present" in {
    MyMap(List(1 -> "one", 2 -> "two")).getWithIndex(3) should equal(Option.empty[(String, Int)])
  }

  "getOrElse" can "return value if present" in {
    MyMap(List(1 -> "one", 2 -> "two")).getOrElse(2, "nope") should equal("two")
  }

  it can "return default if not present" in {
    MyMap(List(1 -> "one", 2 -> "two")).getOrElse(3, "nope") should equal("nope")
  }

  "values" can "return underlying list of values" in {
    MyMap(List(1 -> "one", 2 -> "two")).values should equal(List("one", "two"))
  }
}