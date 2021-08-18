package scala_cats.chapter04.ex_4_3_1

import scala_cats.UnitSpec
import IdMonadOps._

class IdMonadOpsSpec extends UnitSpec {
  "IdMonadOps" can "implement pure" in {
    assert(IdMonadOps.pure(123) === 123)
  }

  it can "implement map" in {
    assert(IdMonadOps.map(pure(123))(_ * 2) === 246)
  }

  it can "implement flatmap" in {
    assert(IdMonadOps.flatMap(pure(123))(_ * 2) === 246)
  }
}