package scala_cats.chapter04.ex_4_6_5

import scala_cats.UnitSpec
import FoldEval.{foldRightSafe, foldRightTextbook}

class FoldEvalSpec extends UnitSpec {
  "foldSafeRight" can "sum a long list of numbers safely" in {
    assert(foldRightSafe((1 to 100000).toList, 0L)(_ + _).value == 5000050000L)
    assert(foldRightTextbook((1 to 100000).toList, 0L)(_ + _).value == 5000050000L)
  }
}