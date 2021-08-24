package scala_cats.chapter04

import scala_cats.UnitSpec
import CustomMonad._
import workouts.OptionCustomMonad.optionMonad

class CustomMonadSpec extends UnitSpec {
  "retry" can "terminate" in {
    assert(retry[Option, Int](3) {
      a => if (a == 0)
        None
      else
        Some(a - 1)
    }.isEmpty)
  }

  ignore can "blow up" in {
    // stack overflow
    assert(retry[Option, Int](100000)(a => if (a == 0) None else Some(a - 1)).isEmpty)
  }

  "retryTailRecM" can "terminate even for a large recursion count" in {
    // stack overflow
    assert(retryTailRecM[Option, Int](100000)(a => if (a == 0) None else Some(a - 1)).isEmpty)
  }

  "retryM" can "terminate even for a large recursion count" in {
    // stack overflow
    assert(retryM[Option, Int](100000)(a => if (a == 0) None else Some(a - 1)).isEmpty)
  }
}