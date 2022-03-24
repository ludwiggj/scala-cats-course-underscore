package scala_cats.casestudies.validation.take4

import KleisliFun._
import cats.syntax.validated._ // for valid and invalid
import scala_cats.UnitSpec

class KleisliSpec extends UnitSpec {

  "step 1" can "convert Int into List[Int]" in {
    assert(step1.run(20) == List(21, 19))
  }

  "step 2" can "convert Int into List[Int]" in {
    assert(step2.run(20) == List(20, -20))
  }

  "step 3" can "convert Int into List[Int]" in {
    assert(step3.run(20) == List(40, 10))
  }

  "step 1 andThen step 2" can "convert Int into List[Int]" in {
    assert((step1 andThen step2).run(20) == List(21, -21, 19, -19))
  }

  "step 1 andThen step 2 andThen step 3" can "convert Int into List[Int]" in {
    assert((step1 andThen step2 andThen step3).run(20) == List(42, 10, -42, -10, 38, 9, -38, -9))
  }

  val pred = Predicate.pure((i: Int) => if (i % 2 == 0) i.valid else s"$i is not even".invalid)

  "predicate" can "return valid for even number" in {
    assert(pred.apply(2) == 2.valid)
  }

  it can "return invalid for odd number" in {
    assert(pred.apply(1) == "1 is not even".invalid)
  }

  "predicate run" can "return right for even number" in {
    assert(pred.run().apply(2) == Right(2))
  }

  it can "return left for odd number" in {
    assert(pred.run().apply(1) == Left("1 is not even"))
  }
}