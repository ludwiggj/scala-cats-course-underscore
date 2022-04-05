package scala_cats.casestudies.cdrt.take3

import scala_cats.UnitSpec

import KeyValueStore.mapInstance

class KeyValueStoreSpec extends UnitSpec {

  "kvs get" can "return value if present" in {
    KeyValueStore[Map].get(Map("1" -> 1))("1") should equal(Some(1))
  }

  it can "return none if key missing" in {
    KeyValueStore[Map].get(Map("1" -> 1))("2") should equal(Option.empty[Int])
  }

  "kvs getOrElse" can "return value if present" in {
    KeyValueStore[Map].getOrElse(Map("1" -> 1))("1", 0) should equal(1)
  }

  it can "return default if key missing" in {
    KeyValueStore[Map].getOrElse(Map("1" -> 1))("2", 0) should equal(0)
  }

  "kvs put" can "put element in map" in {
    KeyValueStore[Map].put(Map.empty[String, Int])("1", 2) should equal(Map("1" -> 2))
  }

  it can "put different elements in different maps" in {
    val kvs = KeyValueStore[Map]
    kvs.put(Map.empty[String, Int])("1", 2) should equal(Map("1" -> 2))
    kvs.put(Map(true -> "yes"))(false, "no") should equal(Map(true -> "yes", false -> "no"))
  }

  "values" can "return map values in a list" in {
    KeyValueStore[Map].values(Map(true -> "yes", false -> "no")) should equal(List("yes", "no"))
  }
}