package scala_cats.casestudies.cdrt.take4

import scala_cats.UnitSpec

class KeyValueStoreMoreRestrictiveSpec extends UnitSpec {

  "restrictive kvs put" can "put different elements in different maps" in {
    val rkvs = KeyValueStoreMoreRestrictive[Map, String, Int]
    rkvs.put(Map.empty[String, Int])("1", 2) should equal(Map("1" -> 2))

    // Won't compile as types don't line up
    // rkvs.put(Map(true -> "yes"))(false, "no") should equal(Map(true -> "yes", false -> "no"))
  }
}